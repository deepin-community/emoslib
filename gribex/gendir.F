C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C


      SUBROUTINE GENDIR (HPARAM,KARRAY,KLEN,HLINE,KIN,KOUT,HLAST,KRET)
C
C---->
C**** GENDIR - Generate directive lines for MARS.
C
C     Purpose.
C     --------
C
C           Creates mars directive line(s) in character format
C           from list of integers.
C
C**   Interface.
C     ----------
C
C           CALL GENDIR (HPARAM,KARRAY,KLEN,HLINE,KIN,KOUT,HLAST,KRET)
C
C           Integer    K.
C           Real       P.
C           Logical    O.
C           Character  H.
C
C               Input parameters.
C               -----------------
C
C               HPARAM     - MARS parameter being used.
C               KARRAY     - Array of required values.
C               KLEN       - Number of values.
C               HLINE      - Array of 80 character variables.
C               KIN        - Dimension of array HLINE.
C               HLAST      - Terminating character for directive line(s)
C               KRET       - Abort/no abort when error found.
C                            0   , abort if error found.
C                            Non-zero , no abort on error.
C
C               Output parameters.
C               ------------------
C
C               LINE       - MARS directive line(s)
C               KOUT       - Number of lines generated.
C               KRET       - Error return code.
C                            0   , No error encountered.
C                            1   , Output character array too small.
C
C     Method.
C     -------
C
C           Integers converted to characters and packed to remove
C           spaces. Lines are broken at / character for MARS input.
C
C     Externals.
C     ----------
C
C           RTB
C           REMSP
C
C     Reference.
C     ----------
C
C           MARS User Guide for directive description.
C
C     Comments.
C     ---------
C
C           See also GENINT which performs the reverse function.
C           Program contains sections 0 to 2 and section 9.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      16:10:86.
C
C     Modifications.
C     --------------
C
C           J. Hennessy      ECMWF      10:07:91.
C           Print of error message added.
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
      INTEGER RTB
C
C
      CHARACTER*80  HLINE
      CHARACTER*10  YTEMP
      CHARACTER*(*) HPARAM
      CHARACTER*1   HLAST
C
      DIMENSION KARRAY(*)
      DIMENSION HLINE(KIN)
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
C*    Section 1 . Set initial values.
C     ----------------------------------------------------------------
C
 1000 CONTINUE
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
C*    Section 2 . Convert integer data to required character format.
C     ----------------------------------------------------------------
C
 2000 CONTINUE
C
      KOUT                     = 1
      HLINE(KOUT)(1:)          = HPARAM(1:)
      ILEN                     = RTB (HLINE(KOUT))
      ICHAR                    = ILEN + 1
      HLINE(KOUT)(ICHAR:ICHAR) = '='
      ICHAR                    = ICHAR + 1
C
      DO 2010 I=1,KLEN
         WRITE (YTEMP(1:10),'(I10)') KARRAY(I)
         CALL REMSP (YTEMP)
         ILEN  = RTB (YTEMP)
         ILAST = ILEN + ICHAR
         IF (ILAST.GT.80)
     C      THEN
                ICHAR = 1
                KOUT  = KOUT + 1
                IF (KOUT.GT.KIN)
     C             THEN
                       KRET = 1
                       WRITE (*,9001) KIN , KOUT
                       GO TO 9000
                   ENDIF
            ENDIF
         HLINE(KOUT)(ICHAR:) = YTEMP(1:ILEN)
         ICHAR               = ICHAR + ILEN
         HLINE(KOUT)(ICHAR:) = '/'
         ICHAR               = ICHAR + 1
 2010 CONTINUE
C
      ILEN                   = RTB (HLINE(KOUT))
      HLINE(KOUT)(ILEN:ILEN) = HLAST
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
C
C
C*    Section 9 . Return to calling routine or abort.
C     ------------------------------------------------------------------
C
 9000 CONTINUE
C
 9001 FORMAT (1H ,'GENDIR : Character array is dimensioned ',I3,
     C            ' but should be at least ',I3,'.')
C
      IF (KRET.NE.0.AND.IRET.EQ.0) CALL ABORT
C
      RETURN
C
      END
