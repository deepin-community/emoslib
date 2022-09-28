C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C


      SUBROUTINE LNBFCR (KARRAY,KLENB)
C
C---->
C**** LNBFCR - Calculate length of BUFR message on Cray.
C
C     Purpose.
C     --------
C
C           Calculate length of BUFR message.
C
C**   Interface.
C     ----------
C
C           CALL LNBFCR (KARRAY,KLENB)
C
C           Integer    K.
C           Real       P.
C           Logical    O.
C           Character  H.
C
C               Input parameters.
C               -----------------
C
C               KARRAY     - Array containing BUFR message.
C
C               KLENB      - 0, abort if error.
C                            1, return to calling routine even if error,
C                               unless array limits exceeded.
C
C               Output parameters.
C               ------------------
C
C               KLENB      - Length of BUFR message in bytes.
C                            -1, error in calculating length.
C
C     Method.
C     -------
C
C           The length of each BUFR message is calculated
C           from the BUFR header Sections.
C
C     Externals.
C     ----------
C
C           UNPACK
C
C     Reference.
C     ----------
C
C           WMO BUFR Code Documentation.
C
C     Comments.
C     ---------
C
C           Routine contains Sections 0 to 1 and Section 9.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      20:06:90.
C
C     Modifications.
C     --------------
C
C           J. Hennessy      ECMWF      18:12:90.
C           Soft fail option added.
C
C           J. Hennessy      ECMWF      04:02:92.
C           Modified to cater for Edition 2 of BUFR code.
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
      PARAMETER (JPBYT=2056)
C
      DIMENSION KARRAY(*)
      DIMENSION ISCATR(JPBYT)
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
C*    Section 1 . Calculate BUFR length.
C     ----------------------------------------------------------------
C
 1000 CONTINUE
C
      IABORT = KLENB
C
C     Unpack first JPBYT bytes of data.
C
      CALL UNPACK (KARRAY,8,ISCATR,JPBYT)
C
C     BUFR Edition 2 or greater has full message length in
C     bytes 5-7. ( Edition number is in byte 8)
C
      IF (ISCATR(8).GE.2)
     C   THEN
             KLENB = SHIFTL(ISCATR(5),16) + SHIFTL(ISCATR(6),8)
     C                 + ISCATR(7)
             GO TO 9000
         ENDIF
C
C     Length not in message, so calculate.
C     Count of octets in Section 0.
C
      KLENB = 4
C
C     Count of octets in Section 1.
C
      KLENB = KLENB + SHIFTL(ISCATR(5),16) + SHIFTL(ISCATR(6),8)
     C                 + ISCATR(7)
C
C     Check if optional Section 2 is present.
C
      IFLAG = ISCATR(12)
C
C     Check flag value.
C
      IF (IFLAG.NE.0.AND.IFLAG.NE.128)
     C   THEN
             WRITE (*,9002) IFLAG
             IF (IABORT.EQ.0) CALL ABORT
             KLENB = -1
             GO TO 9000
         ENDIF
C
      IF (IFLAG.EQ.128)
     C        KLENB = KLENB + SHIFTL(ISCATR(KLENB+1),16) +
     C                SHIFTL(ISCATR(KLENB+2),8) + ISCATR(KLENB+3)
C
C     Check on array length.
C
      IF (KLENB.GT.JPBYT)
     C   THEN
             WRITE (*,9001)
             IF (IABORT.EQ.0) CALL ABORT
             KLENB = -1
             GO TO 9000
         ENDIF
C
C     Length of Section 3.
C
      KLENB = KLENB + SHIFTL(ISCATR(KLENB+1),16) +
     C                SHIFTL(ISCATR(KLENB+2),8) + ISCATR(KLENB+3)
C
C     Check on array length.
C
      IF ((KLENB+3).GT.JPBYT)
     C   THEN
             WRITE (*,9001)
             IF (IABORT.EQ.0) CALL ABORT
             KLENB = -1
             GO TO 9000
         ENDIF
C
C     Length of binary data Section 4.
C
      KLENB = KLENB + SHIFTL(ISCATR(KLENB+1),16) +
     C                SHIFTL(ISCATR(KLENB+2),8) + ISCATR(KLENB+3)
C
C     Length of Section 5.
C
      KLENB = KLENB + 4
C
C    ----------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C*    Section 9 . Return to calling routine.
C     ------------------------------------------------------------------
C
 9000 CONTINUE
C
 9001 FORMAT (1H ,'LNBFCR : Array ISCATR limits exceeded.')
C
 9002 FORMAT (1H ,'LNBFCR : Error in section 2 flag - ',I4)
C
      RETURN
C
      END
