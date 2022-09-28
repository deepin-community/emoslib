C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE GETIB1 (KLIB1,KIB1,KLENG,KARRAY,KPR,KRET)
C
C---->
C**** GETIB1 - Get values from section 1 of GRIB code.
C
C     Purpose.
C     --------
C
C           Get integer values from section 1 of unpacked GRIB or
C           pseudo-GRIB records, retrieved from MARS.
C
C**   Interface.
C     ----------
C
C           CALL GETIB1 (KLIB1,KIB1,KLENG,KARRAY,KPR,KRET)
C
C           INTEGER    K.
C           REAL       P.
C           LOGICAL    O.
C           CHARACTER  H.
C
C               Input parameters.
C               -----------------
C
C               KIB1       - Array to receive section 1 values.
C               KLENG      - Length of this array.
C               KARRAY     - Array containing unpacked record.
C               KPR        - Debug print switch.
C                            0 , No printout.
C                            1 , Debug printout.
C                            2 , Print section 1 values.
C
C               KRET       - Abort/no abort when error found.
C                            0   , Abort if error found.
C                            Non-zero , no abort when error found.
C
C               Output parameters.
C               ------------------
C
C               KIB1       - Array containing values from section 1.
C               KLIB1      - Number of values in this array.
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
C           PRTBK1
C
C     Reference.
C     ----------
C
C           See MARS format documentation re GRIB, pseudo-GRIB and
C           unpacked formats.
C
C     Comments.
C     ---------
C
C           Program contains sections 0 to 2 and section 9.
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
C*    Section 0 . Definition of variables.
C     ------------------------------------
C
      IMPLICIT LOGICAL     ( L, O, G )
      IMPLICIT CHARACTER*8 ( C, H, Y )
      IMPLICIT INTEGER     ( I, J, K, M, N )
C
      DIMENSION KARRAY(*)
      DIMENSION KIB1(KLENG)
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
C*    Section 1 . Set initial values. Check input parameters.
C     -------------------------------------------------------
C
 1000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'GETIB1 : Section 1.'
C
      IRET = KRET
      KRET = 0
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
C*    Section 2 . Extract values . Print if required.
C     -----------------------------------------------
C
 2000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'GETIB1 : Section 2.'
C
      KLIB1 = KARRAY(2)
C
      IF (KLENG.LT.KLIB1)
     C   THEN
             KRET = 1
             WRITE (*,9001) KRET , KLIB1
             GO TO 9000
         ENDIF
C
      DO 2010 J=1,KLIB1
         KIB1(J) = KARRAY(J+2)
 2010 CONTINUE
C
      IF (KPR.GT.0) CALL PRTBK1(KIB1)
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
C*    Section 9 . Return to calling routine or abort. Format statements.
C     ------------------------------------------------------------------
C
 9000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'GETIB1 : Section 9.'
C
      IF (KRET.NE.0.AND.IRET.EQ.0) CALL ABORT
C
      RETURN
C
C
 9001 FORMAT (1H ,'GETIB1 : Error code = ',I3,'. Target array must be ',
     C             ' at least ',I5,' words long.')
C
C     ------------------------------------------------------------------
C
C
      END
