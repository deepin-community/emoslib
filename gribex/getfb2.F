C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE GETFB2 (KLFB2,PFB2,KLENG,KARRAY,KPR,KRET)
C
C---->
C**** GETFB2 - Extract real values from section 2 of GRIB code.
C
C     Purpose.
C     --------
C
C           Get real values from section 2 of unpacked GRIB or
C           Pseudo-grib records, retrieved from MARS.
C
C**   Interface.
C     ----------
C
C           CALL GETFB2 (KLFB2,PFB2,KLENG,KARRAY,KPR,KRET)
C
C           INTEGER    K.
C           REAL       P.
C           LOGICAL    O.
C           CHARACTER  H.
C
C               Input parameters.
C               -----------------
C
C               PFB2       - Array to receive section 2 values.
C               KLENG      - Length of this array.
C               KARRAY     - Array containing unpacked record.
C               KPR        - Debug print switch.
C                            0 , No printout.
C                            1 , Debug printout.
C                            2 , Print values extracted.
C
C               KRET       - Abort/no abort when error found.
C                            0   , abort if error found.
C                            Non-zero , no abort if error found.
C
C               Output parameters.
C               ------------------
C
C               PFB2       - Array containing values from section 2.
C               KLFB2      - Number of values in this array.
C               KRET       - Error return code.
C                            0   , No error encountered.
C                            1   , Target array too small.
C
C     Method.
C     -------
C
C           Unpacked data is in self defining records. The required
C           values are located and extracted.
C
C     Externals.
C     ----------
C
C           None.
C
C     Reference.
C     ----------
C
C           See MARS format documentation re GRIB, Pseudo-grib and
C           unpacked formats.
C
C     Comments.
C     ---------
C
C           Subroutine contains sections 0 to 2 and section 9.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      18:06:86.
C
C     Modifications
C     --------------
C
C           None.
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
      DIMENSION PFB2(KLENG)
C
      EQUIVALENCE (IVAL,ZVAL)
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
      IF (KPR.EQ.1) WRITE (*,*) 'GETFB2 : SECTION 1.'
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
      IF (KPR.EQ.1) WRITE (*,*) 'GETFB2 : SECTION 2.'
C
      ILIB1 = KARRAY(2)
      ILIB2 = KARRAY(2+ILIB1+1)
      KLFB2 = KARRAY(2+ILIB1+1+ILIB2+1)
C
      IF (KLENG.LT.KLFB2)
     C   THEN
             KRET = 1
             WRITE (*,9001) KRET , KLFB2
             GO TO 9000
         ENDIF
C
      DO 2010 J=1,KLFB2
         IVAL    = KARRAY(J+2+ILIB1+1+ILIB2+1)
         PFB2(J) = ZVAL
         IF (KPR.GT.0) WRITE (*,9501) ZVAL
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
      IF (KPR.EQ.1) WRITE (*,*) 'GETFB2 : SECTION 9.'
C
      IF (KRET.NE.0.AND.IRET.EQ.0) CALL ABORT
C
      RETURN
C
C
 9001 FORMAT (1H ,'GETFB2 : ERROR CODE = ',I3,'. TARGET ARRAY MUST BE ',
     C             ' AT LEAST ',I5,' WORDS LONG.')
C
 9501 FORMAT (1H ,F30.8)
C
C     ------------------------------------------------------------------
C
C
      END
