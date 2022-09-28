C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

#ifdef CRAY
      SUBROUTINE UNBLOCK (UN1,UN2,IRET)
C
C---->
C**********************************************************************
C*                                                                    *
C*    NAME      : UNBLOCK                                             *
C*                                                                    *
C*    FUNCTION  : CONVERTS CRAY BLOCKED DATASET OF GRIB DATA INTO AN  *
C*                UNBLOCKED DATASET                                   *
C*                                                                    *
C*    INPUT     : UN1   = INPUT  LUN                                  *
C*                UN2   = OUTPUT LUN                                  *
C*                                                                    *
C*    OUTPUT    : IRET  = -100   BUFFER IN  ERROR                     *
C*                      = -200   BUFFER OUT ERROR                     *
C*                      > 0      NO. OF BLOCKS WRITTEN                *
C*                                                                    *
C*    GENERAL   : UNBLOCK CALLS   -----                               *
C*                                                                    *
C*    AUTHOR    : B.V. GURETZKY     V.E. DAY                          *
C*                                                                    *
C*    MODIFIED  : J. HENNESSY       26.11.85                          *
C*                                                                    *
C**********************************************************************
C----<
C
      IMPLICIT INTEGER (B,U)
      PARAMETER (BLEN=25088)
      DIMENSION BUF(BLEN+512)
C
C     NO OF WORDS PER BLOCK (UNBLOCKED DATASET ICB = 512 )
      DATA ICB /512/
C
      IRET = 0
      IRES = 0
C
      DO 5000 J = 1,10000
C
C        READ INPUT FILE RECORD.
C
         BUFFER IN (UN1,0) (BUF(IRES+1),BUF(BLEN+ICB))
         IUNIT = UNIT(UN1)
         IF (UNIT(UN1)) 1000,5500,10000
 1000    CONTINUE

         LEN  = IRES + LENGTH(UN1)
         K    = LEN  / ICB
         IE   = K    * ICB
         IRES = LEN  - IE
C
         IF (K.NE.0)
     C      THEN
C
C               WRITE FIRST CHUNK OF ICB-BLOCKS
C
                BUFFER OUT (UN2,0) (BUF(1),BUF(IE))
                IUNIT = UNIT(UN2)
                IF (IUNIT.GT.0) GO TO 10100
                IRET = IRET + K
C
C               MOVE REMAINDER OF BLOCK TO FRONT AND GET NEXT RECORD.
C
                DO 4000 I = 1,IRES
                   BUF(I) = BUF(IE+I)
 4000           CONTINUE
C
            ENDIF
C
 5000 CONTINUE
C
C
C     HANDLE LAST BLOCK, SETTING UNUSED WORDS OF LAST BLOCK TO 0.
C
 5500 IF (IRES.NE.0)
     C   THEN
             IRES = IRES + 1
             DO 6000 I = IRES,ICB
                BUF(I) = 0
 6000        CONTINUE
             BUFFER OUT (UN2,0)(BUF(1),BUF(ICB))
             IUNIT = UNIT(UN2)
             IF (IUNIT.GT.0) GO TO 10100
             IRET = IRET + 1
         ENDIF
C
      RETURN
C
10000 CONTINUE
      IRET = -100
      WRITE (*,*) 'UNBLOCK : BUFFER IN ERROR'
      RETURN
C
10100 CONTINUE
      IRET = -200
      WRITE (*,*) 'UNBLOCK : BUFFER OUT ERROR'
      RETURN
C
      END
#endif
