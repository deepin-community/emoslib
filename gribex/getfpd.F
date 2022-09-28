C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE GETFPD (KFPD,PFPD,KLEN,KARAY,KPR,KRET)
C
C---->
C**** GETFPD - Extract data from unpacked GRIB record.
C
C     Purpose.
C     --------
C
C           Get floating point data from record of unpacked GRIB
C           or Pseudo-grib data.
C
C**   Interface.
C     ----------
C
C           CALL GETFPD (KFPD,PFPD,KLEN,KARAY,KPR,KRET)
C
C           INTEGER    K.
C           REAL       P.
C           LOGICAL    O.
C           CHARACTER  H.
C
C               Input parameters.
C               -----------------
C
C               PFPD       - Array to receive floating point data.
C               KLEN       - Length of this array.
C               KARAY      - Array containing unpacked record.
C
C
C               KPR        - Debug print switch.
C                            0 , No printout.
C                            1 , Debug printout.
C
C               KRET       - Abort/no abort when error found.
C                            0   , Abort if error found.
C                            Non-zero , no abort on error.
C
C               Output parameters.
C               ------------------
C
C               KRET       - Error return code.
C                            0   , No error encountered.
C                            1   , Target array too small.
C
C               PFPD       - Floating point data.
C               KFPD       - Number of values in this array.
C
C     Method.
C     -------
C
C           Unpacked data is in self-defining records. The required
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
C           unpacked record formats.
C
C     Comments.
C     ---------
C
C           Routine contains sections 0 to 2 and section 9.
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
C     ----------------------------------------------------------------
C---->
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
      DIMENSION KARAY(*)
      DIMENSION PFPD(KLEN)
C
      EQUIVALENCE (IVAL,ZVAL)
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
C     ----------------------------------------------------------------
C
 1000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'GETFPD : SECTION 1.'
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
C     ----------------------------------------------------------------
C
 2000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'GETFPD : SECTION 2.'
C
      ILB1 = KARAY(2)
      ILB2 = KARAY(2+ILB1+1)
      ILF2 = KARAY(2+ILB1+1+ILB2+1)
      ILB3 = KARAY(2+ILB1+1+ILB2+1+ILF2+1)
      ILCD = KARAY(2+ILB1+1+ILB2+1+ILF2+1+ILB3+1)
      KFPD = KARAY(2+ILB1+1+ILB2+1+ILF2+1+ILB3+1+ILCD+1)
C
      IF (KLEN.LT.KFPD)
     C   THEN
             KRET = 1
             WRITE (*,9001) KRET , KFPD
             GO TO 9000
         ENDIF
C
      DO 2010 J=1,KFPD
         IVAL    = KARAY(J+2+ILB1+1+ILB2+1+ILF2+1+ILB3+1+ILCD+1)
         PFPD(J) = ZVAL
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
      IF (KPR.EQ.1) WRITE (*,*) 'GETFPD : SECTION 9.'
C
      IF (KRET.NE.0.AND.IRET.EQ.0) CALL ABORT
C
      RETURN
C
C
 9001 FORMAT (1H ,'GETFPD : ERROR CODE = ',I3,'. TARGET ARRAY MUST ',
     C            'BE AT LEAST ',I5,' WORDS LONG.')
C
C
C     ---------------------------------------------------------------
C
C
      END
C
