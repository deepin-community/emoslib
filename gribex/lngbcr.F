C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C


      SUBROUTINE LNGBCR (KARRAY,KLENB)
C
C---->
C**** LNGBCR - Calculate or extract length of GRIB field on Cray.
C
C     Purpose.
C     --------
C
C           Calculate or extract length of GRIB field.
C
C**   Interface.
C     ----------
C
C           CALL LNGBCR (KARRAY,KLENB)
C
C           INTEGER    K.
C           REAL       P.
C           LOGICAL    O.
C           CHARACTER  H.
C
C               Input parameters.
C               -----------------
C
C               KARRAY     - Array containing GRIB field.
C
C               Output parameters.
C               ------------------
C
C               KLENB      - Length of GRIB field in bytes.
C
C     Method.
C     -------
C
C           The length of each GRIB encoded field is calculated
C           from the GRIB header Sections for Experimental Edition
C           or Edition 0 of GRIB code. For GRIB Edition 1 the length
C           is extracted from bytes 5-7 of section 0.
C
C     Externals.
C     ----------
C
C           UNPACK
C
C     Reference.
C     ----------
C
C           WMO GRIB Code Documentation.
C
C           Research Department Documentation for description of
C           BUDG and TIDE Pseudo-Grib messages.
C
C     Comments.
C     ---------
C
C           ECMWF Pseudo-Grib fields (BUDG and TIDE)are handled by this
C           routine.
C
C           Routine contains Sections 0 to 1 and Section 9.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      30:04:90.
C
C     Modifications.
C     --------------
C
C           J. Hennessy      ECMWF      31:05:90.
C           Increase array size to accommodate vertical level
C           co-ordinates of model level data. Add array size check.
C
C           J. Hennessy      ECMWF      13:11:90.
C           Increase array size to accommodate bit maps.
C
C           J. Hennessy      ECMWF      16:05:91.
C           Modified to handle GRIB Edition 1.
C
C           J. Hennessy      ECMWF      23:09:92.
C           Washington SST made fixed length for GRIB Edition 1.
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
C*    Section 0 . Definition of variables.
C     ----------------------------------------------------------------
C
      IMPLICIT LOGICAL     ( L, O, G )
      IMPLICIT CHARACTER*8 ( C, H, Y )
      IMPLICIT INTEGER     ( I, J, K, M, N )
C
      PARAMETER (JPBYT=5440)
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
C
C*    Section 1 . Unpack the data.
C     ----------------------------------------------------------------
C
  100 CONTINUE
C
C     Unpack first JPBYT octets of data, 1 octet per word.
C
      CALL UNPACK (KARRAY,8,ISCATR,JPBYT)
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
C*    Section 2 . Identify GRIB Edition of data.
C     ----------------------------------------------------------------
C
  200 CONTINUE
C
C     In the Experimental GRIB Edition, octet 8 is a flag field
C     and can have a valid value of 0, 1, 2 or 3.
C     In GRIB Edition 0, octet 8 is reserved and set to 0.
C     In GRIB Edition 1, the edition number is in octet 8.
C
C     However, Edition 0 of the code has a fixed length of 24 octets,
C     included in the message, for section 1, so if the value
C     contained in octets 5-7 is 24, and that in octet 8 is 0, it
C     is safe to assume Edition 0 of the GRIB code.
C
      IF (ISCATR(5).EQ.0.AND.ISCATR(6).EQ.0.AND.ISCATR(7).EQ.24.
     C              AND.ISCATR(8).EQ.0)
     C   THEN
             ISNEW = 1
             GO TO 400
         ENDIF
C
C     In the Experimental GRIB Edition, octets 23 and 24 are reserved
C     and set to 0. These octets in Edition 1 are the month and the day,
C     and must be non-zero.
C
      IF (ISCATR(22).EQ.0.AND.ISCATR(23).EQ.0)
     C   THEN
             ISNEW = 0
             GO TO 400
         ENDIF
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
C*    Section 3 . Extract GRIB length from Edition 1 data.
C     ----------------------------------------------------------------
C
  300 CONTINUE
C
C     Edition number is 1, so extract length from octets 5-7.
C
      KLENB = SHIFTL(ISCATR(5),16) + SHIFTL(ISCATR(6),8) + ISCATR(7)
      IF (ISCATR(13).EQ.7.AND.ISCATR(17).EQ.139) KLENB = 34920
      GO TO 900
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
C*    Section 4 . Calculate GRIB length.
C     ----------------------------------------------------------------
C
  400 CONTINUE
C
C     NMC Washington Sea Surface Temperature analysis is a global
C     field made in ECMWF from 8 component fields received on the GTS
C     and coded in GRIB using ECMWF field code 139. This field
C     contains a bit map and varies in length. For archiving purposes
C     a fixed length was used (34920) bytes. NMC has code figure 7
C     as centre of origin. Experimental Edition of GRIB was never
C     used for this product.
C
      IF (ISNEW.EQ.1.AND.ISCATR(9).EQ.7.AND.ISCATR(13).EQ.139)
     C   THEN
             KLENB = 34920
             GO TO 900
         ENDIF
C
C     Count of octets in Section 0.
C
      KLENB = 4
C
C     Count of octets in Section 1 - code edition dependent.
C
      KLENB = KLENB + 20 + (ISNEW*4)
C
C     Check if optional Section 2 is present.
C
      IFLAG = ISCATR(8+4*ISNEW)
      INC   = 0
      IF (ISNEW.EQ.0)
     C   THEN
             IF (IFLAG.EQ.1.OR.IFLAG.EQ.3) INC = 1
         ELSE
             IF (IFLAG.EQ.128.OR.IFLAG.EQ.192) INC = 1
         ENDIF
C
      IF (INC.EQ.1)
     C        KLENB = KLENB + SHIFTL(ISCATR(KLENB+1),16) +
     C                SHIFTL(ISCATR(KLENB+2),8) + ISCATR(KLENB+3)
C
C     Check on array length.
C
      IF (KLENB.GT.JPBYT)
     C   THEN
             WRITE (*,9001)
             CALL ABORT
         ENDIF
C
C     Check if optional Section 3 is present.
C
      INC   = 0
      IF (ISNEW.EQ.0)
     C   THEN
             IF (IFLAG.EQ.2.OR.IFLAG.EQ.3) INC = 1
         ELSE
             IF (IFLAG.EQ.64.OR.IFLAG.EQ.192) INC = 1
         ENDIF
C
      IF (INC.EQ.1)
     C        KLENB = KLENB + SHIFTL(ISCATR(KLENB+1),16) +
     C                SHIFTL(ISCATR(KLENB+2),8) + ISCATR(KLENB+3)
C
C     Check on array length.
C
      IF ((KLENB+3).GT.JPBYT)
     C   THEN
             WRITE (*,9001)
             CALL ABORT
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
  900 CONTINUE
C
 9001 FORMAT (1H ,'LNGBCR : Array ISCATR limits exceeded.')
C
      RETURN
C
      END
