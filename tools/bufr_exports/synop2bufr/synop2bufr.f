       PROGRAM synop2bufr
C
C
c**** *synop2bufr*
c
c
c     PURPOSE.
c     --------
c         Read GTS bulletin from the file
c         and creates WMO 307080 template bufr data
c
c
c**   INTERFACE.
c     ----------
c
c          NONE.
c
c     METHOD.
c     -------
c
c          NONE.
c
c
c     EXTERNALS.
c     ----------
c
c
c     REFERENCE.
c     ----------
c
c          NONE.
c
c     AUTHOR.
c     -------
c
c          MILAN DRAGOSAVAC    *ECMWF*      2009/04/27
c
c
c     MODIFICATIONS.
c     --------------
c
c          NONE.
c
c
c     IMPLICIT NONE
c
c
      include 'cominit.h'
c 
c
      CHARACTER*256 CARG(10)
c
      CHARACTER*256 CINFILE
      CHARACTER*256 COUTFILE

      INTEGER           NARG
      INTEGER           IARGC
      INTEGER           I,J,IO,IN,IERR,K
      INTEGER           KBUFL
      INTEGER           KBUFR(128000)
      INTEGER           IUNIT,IUNIT1
      LOGICAL           FIRST

      CHARACTER*512000  YOUT
      CHARACTER*3       CCCC
c     ------------------------------------------------------------------
 
c     GET INPUT AND OUTPUT FILE NAME.
 
      NCENTRE=0
      NARG=IARGC()
 
      DO J=1,NARG
      CALL GETARG(J,CARG(J))
      END DO

      cinfile=' '
      coutfile=' '

      DO J=1,NARG,2
      IF(CARG(J).EQ.'-i') THEN
         CINFILE=CARG(J+1)
         IN=index(CINFILE,' ')
         IN=IN-1
      ELSEIF(CARG(J).EQ.'-o') THEN
         COUTFILE=CARG(J+1)
         IO=index(COUTFILE,' ')
         IO=IO-1
      ELSEIF(CARG(J).EQ.'-c') THEN
         CCCC=CARG(J+1)
         read(CCCC,'(i3.3)') NCENTRE
      END IF
      END DO
c
      if(in .eq.0 .or.io .eq.0 .or. NCENTRE.eq.0) then
         PRINT*,'USAGE -- synop2bufr -i infile -o outfile -c centre'
         STOP
      END IF
 
c*          1.2 OPEN FILE CONTAINING GTS BULLETIN and BUFR FILE.

 120  CONTINUE

      CALL PBOPEN(IUNIT,CINFILE(1:IN),'R',IERR)
      IF(IERR.EQ.-1) STOP 'OPEN FAILED INPUT FILE'
      IF(IERR.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IERR.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'

      CALL PBOPEN(IUNIT1,COUTFILE(1:IO),'W',IERR)
      IF(IERR.EQ.-1) STOP 'OPEN FAILED ON OUTPUT FILE'
      IF(IERR.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IERR.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'
 
      first=.true.
c     ----------------------------------------------------------------- 
 
c*          3.  READ INPUT FILE
c               ---------------
 300  CONTINUE
 
      KREC=0
      IERR=0
      DO WHILE ( IERR .eq.0)
      K=0
      CALL READ_GTS(IUNIT,YOUT,K,IERR)
      IF(IERR.eq.1) THEN
         ierr=0
         KREC=KREC+1
         print*,'Bulletin number ---------',KREC
!        print*,YOUT(1:len_trim(YOUT))
         CALL DECODE(K,YOUT,IERR)
         if(ierr.ne.0) then
           print*,'Error in decoding bulletin ',ierr
           ierr=0
         end if
         Print*,'The file is processed'
         go to 400
      end if
      KREC=KREC+1
      print*,'Bulletin number ---------',KREC
!     print*,YOUT(1:len_trim(YOUT))
c
      ierr=0
      CALL DECODE(K,YOUT,IERR)
      if(ierr.ne.0) then
         print*,'Error in decoding bulletin ',ierr
         ierr=0
      end if
c
      END DO
c
 400  continue
c400  CALL PBCLOSE(IUNIT,IERR)
c     CALL PBCLOSE(IUNIT1,IERR)

      END 

      SUBROUTINE DECODE(KLEN,YIN,IERR)
C
C

C
C**** *DECODE*
C
C
C     PURPOSE.
C     --------
C         CONTROLLING ROUTINE FOR DECODING
C         DATA.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *DECODE(KLEN,YIN,KBUFL,KBUFR,KERR)*
C                 KLEN  - size in bytes of YIN
C                 YIN   - character string containing one bulletin
C                 KUNIT - output file unit number
C                 KERR  - return error code
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       2009/04/27
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
      INCLUDE 'parameter.h'
      INCLUDE 'combuff.h'
      INCLUDE 'comwork.h'
      INCLUDE 'comrec.h'
      INCLUDE 'compoin.h'
      INCLUDE 'comstat.h'
      INCLUDE 'comkey.h'
C
      CHARACTER*(*) YIN
C
C     ------------------------------------------------------------------
C*          1.   INITIALIZE VARIABLES AND CONSTANTS.
C                ----------------------------------
 100  CONTINUE
C
      IERR=0
C
     
      CALL INITVAR( IERR )
      IF(IERR.NE.0) CALL exit(2)
C
C        Move character string into integer array
C    
         ILEN=KLEN
C
         DO 141 I=1,ILEN
C
         KCHAR(I)=IAND(ICHAR(YIN(I:I)),127)
C
 141     CONTINUE

C        ---------------------------------------------------------------

         CALL SYNOP(IERR)
C
         RETURN

C
C     ------------------------------------------------------------------
C
 1000 CONTINUE
C
      RETURN
      END
      SUBROUTINE SYNOP(IERR)
C
C
C**** *SYNOP*
C 
C
C
C     PURPOSE.
C     --------
C         CONTROLLING ROUTINE FOR DECODING
C         SYNOP DATA.
C
C
C**   INTERFACE.
C     ----------
C
C         NONE.
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C        *CALL* *INITVAR( IERR )*
C        *CALL* *PROCRFB( IERR )*
C        *CALL* *PROCHDR( IERR )*
C        *CALL* *PROCTXT( IERR )*
C        *CALL* *PROCT1S( IERR )*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/08/88.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
      INCLUDE 'combuff.h'
      INCLUDE 'comrec.h'
C
C     ------------------------------------------------------------------
C*          1.   INITIALIZE VARIABLES AND CONSTANTS.
C                ----------------------------------
 100  CONTINUE
C
C     ------------------------------------------------------------------
C*          3.   FORMAT BULLETIN.
C                ----------------
 300  CONTINUE
C
      CALL PROCRFB( IERR )
      IF(IERR.NE.0) RETURN
C     ------------------------------------------------------------------
C*          4.   DECODE BULLETIN HEADER.
C                -----------------------
 400  CONTINUE
C
      CALL PROCHDR( IERR )
      IF(KERR.NE.0) RETURN
C     ------------------------------------------------------------------
C*          5.   CHECK TEXT OF BULLETIN.
C                -----------------------
 500  CONTINUE
C
      CALL PROCTXT( IERR )
      IF(KERR.NE.0) RETURN
C
C     ------------------------------------------------------------------
C*          6.   CALL APPROPRIATE ROUTINE TO DECODE SYNOP DATA.
C                ---------------------------------------------
 600  CONTINUE
C
      CALL PROCT1S( IERR )
C
C     ------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE PROCRFB ( IERR )
C
C
C**** *PROCRFB*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO FORMAT BULLETIN.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PROCRFB(IERR)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C          J. HENNESSY         *ECMWF*
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
      INCLUDE 'parameter.h'
      INCLUDE 'combuff.h'
      INCLUDE 'comwork.h'
      INCLUDE 'comindx.h'
      INCLUDE 'comstat.h'
C     ------------------------------------------------------------------
C*          1.   KEEP SOURCE OF DATA AND DATE/TIME OF ARRIVAL.
C                ---------------------------------------------
 100  CONTINUE
C
      DO 101 I=1,3
C
      KHEAD(I)=MINDIC      ! THIS WILL BE CHANGED ACCORDING TO THE KEY.
C
 101  CONTINUE
C     ------------------------------------------------------------------
C*          2.  DEFINE T1 AND T2 FROM ABBREVIATED HEADING.
C               ------------------------------------------
 200  CONTINUE

      DO 202 I=1,ILEN
C
      IF(KCHAR(I).GE.65.AND.KCHAR(I).LE.90) GO TO 203
C
 202  CONTINUE
C
      IT1=27
      GO TO 210
C
 203  CONTINUE
C
      IT1=KCHAR(I  )-64
      IT2=KCHAR(I+1)-64
C
      IF(IT1.NE.19) THEN
                       IERR=1
                       IT1=27
                       RETURN
                    END IF
C
C     CHECK IF 'T2' CHARACTER IS LETTER.
C
      IF(IT2.LT.1.OR.IT2.GT.26) IT1=27
C
C*          2.1  LAST CHARACTER OF BULLETIN CAN BE IN ANY OF THE LAST
C                -----------------------------------------------------
C                5 WORDS. IF CHARACTER  IS 'ETX' REPLACE BY 'GS' .
C                -------------------------------------------------
C                IF NEITHER CAN BE FOUND INSERT 'GS' AS LAST CHARACTER.
C                ------------------------------------------------------
 210  CONTINUE
C
      IST=ILEN-5
C
      DO 211 I=1,ILEN
C
      IF (KCHAR(I).EQ.3.OR.KCHAR(I).EQ.29) THEN
                                              KCHAR(I)= 29
                                              IGS=I
                                              RETURN
                                           END IF
C
 211  CONTINUE
C
      I=I-1
      KCHAR(I)= 29
      IGS=I
C
      RETURN
      END
      SUBROUTINE PROCTXT ( IERR )
C
C
C**** *PROCTXT*
C
C
C     PURPOSE.
C     --------
C          CHECKS WHETHER BULLETIN CONTAINS USEFUL DATA .
C          THE FOLLOWING BULLETINS ARE CONSIDERED TO CONTAIN
C          NO USEFUL DATA.
C                 1. TEXT OF 'NIL' , OR VARIANTS OF THIS.
C                 2. TEXT OF 'NO DATA AVAILABLE'.
C                 3. TEXT OF 'NO REPORTS AVAILABLE'.
C                      1. - 3.  ARE DETERMINED SIMPLY BY CHECKING
C                      THE LENGTH OF THE TEXT . IF IT IS LESS
C                      THAN 26 THERE CANT BE ANY USEFUL DATA IN IT
C                 4. UK AND GERMAN DOMESTIC BULLETINS WHICH DO
C                    NOT CONFORM TO WMO CODES.
C
C          INPUT     : BULLETIN IN ARRAY 'KCHAR' ,
C                      ONE CHARACTER PER WORD.
C
C          OUTPUT    : KERR = 0 INDICATES BULLETIN CONTENTS REQUIRED.
C                           = 1 MEANS TEXT OF 'NIL' ETC.
C                           = 2 UK OR GERMAN DOMESTIC BULLETIN.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PROCTXT(IERR)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *NEXTLET(I,J)*
C         *CALL* *PRTBULL(I,J)*
C         *CALL* *SAVBULL(IERR)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/08/88.
C          J. HENNESSY         *ECMWF*
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
      INCLUDE 'comindx.h'
      INCLUDE 'comstat.h'
      INCLUDE 'combuff.h'
C
C     ------------------------------------------------------------------
C
C*          1.   CLEAR ERROR INDICATOR.
C                ----------------------
 100  CONTINUE
C
      KERR= 0
C
C
C*          1.1  CHECK IF BULLETIN IS TOO SHORT I.E. "NIL" BULLETIN.
C                ---------------------------------------------------
 110  CONTINUE
C
      ILE = IGS - IMI
      IF(ILE .LT. 26) THEN
                         KERR = 7
                         NUMBERR(7) = NUMBERR(7) + 1
c                               KCHAR(IGS)=KCHAR(IGS).OR.128
                                KCHAR(IGS)=IAND(KCHAR(IGS),128)
C                               GO TO 900
                      END IF
C
C
C*          1.2  CHECK IF THE BULLETIN IS DOMESTIC FROM UK OR GERMANY.
C                -----------------------------------------------------
C                THIS IS DONE BY CHECKING IF THE BULLETIN HAS
C                --------------------------------------------
C                CCCC OF (EG--,ED--) OTHER THAN (EGRR,EDZW).
C                -------------------------------------------
 120  CONTINUE
C
      IPT=IAH+4
      CALL NEXTLET(IPT,JAH)
C
C     IF BULLETINS HAVE 'CCCC' NOT 'ED--' OR 'EG--' , RETURN.
C
      IF ( KCHAR(IPT).NE.69 ) RETURN
      IF ( KCHAR(IPT+1).NE.71.AND.KCHAR(IPT+1).NE.68 ) RETURN
C
C        FIRST UK
C
             IF (KCHAR(IPT+1).EQ.71)
     C                      THEN
                                 IF(KCHAR(IPT+2) .NE. 82 .OR.
     1                              KCHAR(IPT+3) .NE. 82)
     2                                   KERR = 8
                            ELSE
C
C        THEN GERMANY
C
                                 IF(KCHAR(IPT+2) .NE. 90 .OR.
     1                              KCHAR(IPT+3) .NE. 87)
     2                                   KERR = 8
                            END IF
C
C     MARK CCCC GROUP IF REQUIRED.
C
      IF ( KERR.EQ.8 ) THEN
                              KCHAR(IPT+4) = IOR(KCHAR(IPT+4),128)
                              NUMBERR(8) = NUMBERR(8) + 1
                          END IF
C
C           1.3 TREAT IN ACCORDANCE WITH DEFINED OPTIONS.
C               -----------------------------------------
 130  CONTINUE
C
C     RETURN IF NO ERROR.
C
      IF ( KERR.EQ.0 ) RETURN
C
C
 900  CONTINUE
C
      N = KERR - 1
      N1 =IAND(ISHFT(IOPTS(677),-N),1)
      N2 =IAND(ISHFT(IOPTS(678),-N),1)
C
C           1.4  PRINT BULLETIN IF REQUIRED.
C                ---------------------------
 140  CONTINUE
C
      IF (N1.EQ.1) THEN
                       WRITE (*,9900) KERR
                       CALL PRTBULL (1,IGS)
                   END IF
C
C           1.5 WRITE TO ERROR FILE IF REQUIRED.
C               --------------------------------
 150  CONTINUE
C
      IF ( N2.EQ.1 ) CALL SAVBULL(IERR)
      RETURN
C     -----------------------------------------------------------------
 9900 FORMAT (1H ,'BULLETIN ERROR NUMBER ',I2.2)
C     -----------------------------------------------------------------
      END
      SUBROUTINE PROCHDR ( IERR )
C
C
C**** *PROCHDR
C
C
C     PURPOSE.
C     --------
C         DEODE BULLETIN HEADER AND INSERT REQUIRED PARAMETERS
C         IN DECODED REPORT HEADER.
C
C         LOCATES BEGINNING  AND END OF ABBREVIATED HEADER AND
C         'MIMIMJMJ' LINES.
C
C         INPUT     : BULLETIN IN KCHAR(1) - KCHAR(IGS)
C
C                     BULLETIN RECORD HEADER IN KINT(1) - KINT(5)
C
C                     'IT1' = 27 INDICATING BULLETIN HAS NOT BEEN
C                            IDENTIFIED FROM 'TT' OF ABBREVIATED HEADER.
C
C         OUTPUT    : KDEC(10) = DAY OF MONTH ( INTEGER ) . YY
C                     KDEC(11) = TIME OF BULLETIN - HOURS ( INTEGER ) . G
C                     KDEC(12) = TIME OF BULLETIN - MINS ( INTEGER ) . GG
C
C                     KDEC(14) = 0 IF ORIGIN OF REPORT IS FGGE.
C                                1  "   "     "    "    " BRACKNELL.
C                                2  "   "     "    "    " OFFENBACH.
C
C                     KDEC(20) =1 NIL
C                     KDEC(21) =1 IF BULLETIN IS 'COR' , OTHERWISE
C                                    = 0 .
C
C                     KDEC(21) = 1 IF BULLERIN IS 'CCA'
C                     KDEC(21) = 2 IF BULLERIN IS 'CCB'
C                     KDEC(21) = 3 IF BULLERIN IS 'CCC'
C                     KDEC(21) = 4 IF BULLERIN IS 'CCD'
C                     .
C                     .
C
C                     KDEC(18) = DATE OF BULLETIN ARRIVAL ( ON VAX )
C                     KDEC(19) = TIME  "    "        "      "     "
C
C                     IAH =    "      " BEGINNING OF 'ABBREVIATED HEADER'
C                     JAH =    "      "     END   "       "         "
C
C                     IMI =    "      " BEGINNING OF 'MIMIMJMJ' LINE.
C                     JMI =    "      "     END   "       "       "
C
C                     KERR = 0 IF NO ERROR FATAL TO DECODING ENCOUNTERED.
C                         = 1 IF BULLETIN HAS LESS THAN 3 LINES.
C                         = 2 IF BULLETIN IS NOT RECOGNISED.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PROCHDR( IERR )*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *NEXTPRT(I,J)*
C         *CALL* *NEXTEND(I,J)*
C         *CALL* *NEXTFIG(I,J)*
C         *CALL* *EXTGRP (I,N1,N2,N3,N4,N5,N,IRET)*
C         *CALL* *NEXTLET(I,J)*
C         *CALL* *SAVBULL(IERR )*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C          J. HENNESSY         *ECMWF*
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
      INCLUDE 'combuff.h'
      INCLUDE 'comindx.h'
      INCLUDE 'comstat.h'
C
      DIMENSION ILST(26)
      DATA ILST/65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
     1          81,82,83,84,85,86,87,88,89,90/
C     ------------------------------------------------------------------
C
C*          1.   CLEAR ERROR INDICATOR AND SET REPORT HEADER AREA
C                ------------------------------------------------
C                TO MISSING DATA INDICATOR.
C                --------------------------
 100  CONTINUE
C
      KERR=0
C
      DO 101 I=1,24
C
      KDEC(I)= MINDIC
C
 101  CONTINUE
C
C*          1.1  FLAG FIELDS SET TO ZERO.
C                ------------------------
 110  CONTINUE
C
      KDEC(13)=0
      KDEC(15)=0
      KDEC(21)=0
C
C*          2.  LOCATE BEGINNING AND END OF 'STARTING LINE' ,
C               ---------------------------------------------
C               'ABBREVIATED HEADER' AND 'MIMIMJMJ LINE ' .
C               -------------------------------------------
C
      ISL = 1
      CALL NEXTPRT ( ISL,IGS )
      JSL = ISL
      CALL NEXTEND ( JSL,IGS )
      IAH = JSL
      CALL NEXTPRT ( IAH,IGS ) 
      JAH =IAH
      CALL NEXTEND ( JAH,IGS )
      IMI = JAH
      CALL NEXTPRT ( IMI,IGS )
      JMI = IMI
      CALL NEXTEND ( JMI,IGS )
C
C*          2.1 IF THESE 3 LINES CANNOT BE LOCATED , BULLETIN CONSISTS
C               ------------------------------------------------------
C               OF LESS THAN 3 LINES.
C               ---------------------
 210  CONTINUE
C
      IF ( JMI.GT.IGS ) THEN
C
C                           SET ERROR NUMBER AND MARK ERROR.
C
                            KERR = 1
                            KCHAR(IGS)=IOR(KCHAR(IGS),128)
                            NUMBERR(1) = NUMBERR(1) + 1
                            GO TO 300
                        END IF
C
C
C
C*          2.2  BULLETIN CANNOT BE IDENTIFIED FROM 'TT' .
C                -----------------------------------------
 220  CONTINUE
C
      IF ( IT1.EQ.27 ) THEN
C
C                          SET ERROR NUMBER AND MARK ERROR.
C
                           KERR = 2
                           KCHAR(IAH+2)=IOR(KCHAR(IAH+2),128)
                           NUMBERR(2) = NUMBERR(2) + 1
                           GO TO 300
                       END IF
C
C
C*          2.3  NO CHECKS ARE MADE ON TTAAII OR CCCC GROUPS.
C                --------------------------------------------
 230  CONTINUE
C
C
C
C*          2.4  LOCATE AND DECODE 'YYGGGG' GROUP .
C                ----------------------------------
 240  CONTINUE
C
C     SCAN 'KCHAR' FOR FIRST FIGURE AFTER 'II' FIGURES.
C
      IPT = IAH + 6
      CALL NEXTFIG ( IPT,JAH )
      IF ( IPT.GE.JAH ) THEN
                          KERR = 5
                        ELSE
C
C                         EXTRACT YY,GG AND GG AND CONVERT TO INTEGERS
C                         IN WORDS 10-12 OF 'KINT' .
C
                          CALL EXTGRP( IPT,2,2,2,0,0,10,IRET )
                          IPT = IABS(IPT)
C
C                         TEST VALIDITY OF YY,GG AND GG.
C                         THIS TEST MAKES CHECKING RETURN CODE 'IRET'
C                         UNNECESSARY.
C
                          IF ( KINT(10).LT.1.OR.KINT(10).GT.31 )
     C                             THEN
                                        KERR = 5
                                        KINT(10) = MINDIC
                                   END IF
C
                          IF ( KINT(11).LT.0.OR.KINT(11).GT.23 )
     C                             THEN
                                        KERR = 5
                                        KINT(11) = MINDIC
                                   END IF
C
                          IF ( KINT(12).LT.0.OR.KINT(12).GT.59 )
     C                             THEN
                                        KERR = 5
                                        KINT(11) = MINDIC
                                   END IF
C
C
C                                  COPY TO DECODED REPORT HEADER AREA
C                                  AND RESET WORDS IN 'KINT'.
C
                                   DO 241 I=10,12
                                          KDEC(I) = KINT(I)
                                          KINT(I) = MINDIC
  241                              CONTINUE
C
                        END IF
C
C     MARK ERROR IN YYGGGG GROUP.
C
      IF ( KERR.EQ.5 ) THEN
                              KCHAR(IPT)=IOR(KCHAR(IPT),128)
                              NUMBERR(5) = NUMBERR(5) + 1
                          END IF
C
C
C
C
C*          2.5 THE ONLY CHECK ON 'BBB' IS FOR 'COR'.
C               -------------------------------------
 250  CONTINUE
C
C     FIND NEXT LETTER IN ABBREVIATED HEADER LINE AND CHECK IF 'C' (67)
C
C      CALL PRTBULL(1,IGS)
      CALL NEXTLET ( IPT,JAH )
C
      IF ( KCHAR(IPT).EQ.67.AND.KCHAR(IPT+1).EQ.79) THEN
                                                       KDEC(21)=1
                                                       GO TO 260
                                                    END IF
      IF ( KCHAR(IPT).EQ.67.AND.KCHAR(IPT+1).EQ.67)
     1     THEN
              DO 251 IJ=1,26
              IF(KCHAR(IPT+2).EQ.ILST(IJ)) THEN
                                              KDEC(21)=IJ
                                              GO TO 260
                                           END IF
C
 251          CONTINUE
C
           END IF
C
C
C
C*          2.6 INSERT ORIGIN AND DATE/TIME OF ARRIVAL OF BULLETIN.
C               ---------------------------------------------------
 260  CONTINUE
C
C     ORIGIN IS DERIVED FROM KEY . BRACKNELL FILE
C     NAMES START WITH 'B' AND OFFENBACH WITH 'C'. THIS FILENAME IS
C     IN ASCCI CODE       ( 'B' =66 , 'C' = 67)
C
      KDEC(14) = MINDIC    !    ORIGIN WILL BE DEFINED FROM KEY.
C
C
      KDEC(18) = MINDIC               ! DATE AND TIME OF ARRIVAL
      KDEC(19) = MINDIC               !
C
C
C
C*          2.7 TREAT ERROR BULLETIN IN ACCORDANCE WITH DEFINED OPTIONS.
C               --------------------------------------------------------
 270  CONTINUE
C
C     RETURN IF NO ERRORS FOUND.
C
      IF ( KERR.EQ.0 ) RETURN
C
C
C*          3. HANDLE ERROR BULLETIN.
C              ----------------------
 300  CONTINUE
C
      N = KERR-1
      N1 =IAND(ISHFT(IOPTS(677),-N),1)
      N2 =IAND(ISHFT(IOPTS(678),-N),1)
C
C*          3.1 PRINT BULLETIN IF REQUIRED.
C               ---------------------------
 310  CONTINUE
C
      IF ( N1.EQ.1 ) THEN
                         WRITE (*,9900) KERR
                         CALL PRTBULL ( 1,IGS)
                     END IF
C
C*          3.2 WRITE BULLETIN TO ERROR FILE IF REQUIRED.
C               -----------------------------------------
 320  CONTINUE
C
      IF ( N2.EQ.1 ) CALL SAVBULL(IERR)
C
C*          3.3  ONLY ERRORS 1 AND 2 ARE FATAL TO DECODING , SO CLEAR
C                -----------------------------------------------------
C                ERROR INDICATOR BEFORE RETURNING.
C                ---------------------------------
 330  CONTINUE

      IF ( KERR.GT.2 ) KERR = 0
C
C
      RETURN
C
C
 9900 FORMAT (1H ,'BULLETIN ERROR NUMBER ',I2.2)
C
C
      END
      SUBROUTINE PROCT1S ( IERR )
C
C
C**** *PROCT1S*
C
C
C     PURPOSE.
C     --------
C         CONTROLLING ROUTINE FOR DECODING SURFASE
C         DATA ( BULLETINS WITH 'T1' OF 'S' )
C
C         INPUT    : IERR IS NOT USED ON INPUT.
C
C                    IT2  = 1-26 CORRESPONDING TO 'T2' OF A-Z.
C
C         OUTPUT   : IERR IS UNALTERED UNLESS A FATAL ERROR OCCURRS ,
C                     WHEN IT IS SET TO 1.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PROCT1S(IERR)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *BULLSM(IERR)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
C
C     ------------------------------------------------------------------
C*          1.  CALL APPROPRIATE ROUTINE, 'IT2' CONTAINS AN INTEGER IN THE
C                ---------------------------------------------------------
C               RANGE 1-26, CORRESPONDING TO 'T2' OF ABBREVIATED HEADER.
C                -------------------------------------------------------
 100  CONTINUE
C
C
      GOTO ( 110,120,130,140,150,160,170,180,190,200,210,220,230,
     C       240,250,260,270,280,290,300,310,320,330,340,350,360) IT2
C
C     -----------------------------------------------------------------
C*              1.1  BULLETINS WITH 'TT' = 'SA'.
C                    ----------------------------------------------
 110  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.2  BULLETINS WITH 'TT' = 'SB'.
C                    ---------------------------
 120  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.3  BULLETINS WITH 'TT' = 'SC'.
C                    ---------------------------
  130 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.4  BULLETINS WITH 'TT' = 'SD'.
C                    ---------------------------
  140 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.5  BULLETINS WITH 'TT' = 'SE'.
C                    ----------------------------
  150 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.6  BULLETINS WITH 'TT' = 'SF'.
C                    ---------------------------
  160 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.7  BULLETINS WITH 'TT' = 'SG'.
C                    ----------------------------
  170 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.8  BULLETINS WITH 'TT' = 'SH'.
C                    ---------------------------
  180 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.9  BULLETINS WITH 'TT' = 'SI'. SYNOP INTERMED.HOURS.
C
  190 CONTINUE
      CALL BULLSI( IERR )
      RETURN
C     -----------------------------------------------------------------
C*              2.0  BULLETINS WITH 'TT' = 'SJ'.
C                    ---------------------------
 200  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.1  BULLETINS WITH 'TT' = 'SK'.
C                    ---------------------------
 210  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.2  BULLETINS WITH 'TT' = 'SL'.
C                    ---------------------------
 220  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.3  BULLETINS WITH 'TT' = 'SM'. SYNOP MAIN HOURS.
C                    ---------------------------
 230  CONTINUE
      CALL BULLSM( IERR )
      RETURN
C     -----------------------------------------------------------------
C*              2.4  BULLETINS WITH 'TT' = 'SN'. NON-STANDARD HOUR.
C                    ---------------------------
 240  CONTINUE
      CALL BULLSN( IERR )
      RETURN
C     -----------------------------------------------------------------
C*              2.5  BULLETINS WITH 'TT' = 'SO'.
C                    ---------------------------
 250  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.6  BULLETINS WITH 'TT' = 'SP'.
C                    ---------------------------
 260  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.7  BULLETINS WITH 'TT' = 'SQ'.
C                    ---------------------------
 270  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.8  BULLETINS WITH 'TT' = 'SR'.
C                    ---------------------------
 280  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.9  BULLETINS WITH 'TT' = 'SS'.
C                    ---------------------------
 290  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              3.0  BULLETINS WITH 'TT' = 'ST'.
C                    ---------------------------
 300  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              3.1  BULLETINS WITH 'TT' = 'SU'.
C
 310  CONTINUE
      RETURN
C     ------------------------------------------------------------------
C*              3.2  BULLETINS WITH 'TT' = 'SV'.
C
 320  CONTINUE
      RETURN
C     ------------------------------------------------------------------
C*              3.3  BULLETINS WITH 'TT' = 'SW'.
C
 330  CONTINUE
      RETURN
C     ------------------------------------------------------------------
C*              3.4  BULLETINS WITH 'TT' = 'SX'.
C
 340  CONTINUE
      RETURN
C     ------------------------------------------------------------------
C*              3.5  BULLETINS WITH 'TT' = 'SY'.
C
 350  CONTINUE
      RETURN
C     ------------------------------------------------------------------
C*              3.6  BULLETINS WITH 'TT' = 'SZ'.
C
 360  CONTINUE
      RETURN
C
      END
      SUBROUTINE BULLSM ( IERR )
C
C
C**** *BULLSM*
C
C
C     PURPOSE.
C     --------
C
C         CONTROLLING ROUTINE FOR DECODING OF INDIVIDUAL
C         REPORTS FROM BULLETINS OF SURFACE OBSERVATIONS.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *BULLSM(IERR)*
C
C          INPUT      : BULLETIN IN CCITT 5 CHARACTERS , 1 CHARACTER PER
C                       WORD IN 'KCHAR' .
C
C                       POINTERS TO BEGINNING AND END OF 'STARTING LINE' ,
C                       'ABBREVIATED HEADING','MIMIMJ LINE' AND END OF
C                       BULLETIN .
C
C                       IERR NOT USED.
C
C          OUTPUT     : DECODED REPORTS WRITTEN TO FILE AND ERROR
C                       REPORTS TO ERROR FILE.
C
C                       IERR = 1 , IF ANY FILE HANDLING ERROR .
C
C
C
C     METHOD.
C     -------
C
C         THIS ROUTINE HAS 3 ENTRY POINTS . SYNOP AND SHIP
C         REPORTS FOR MAIN,INTERMEDIATE AND NON-STANDARD HOURS
C         ARE DECODED BY THIS ROUTINE.
C
C         THIS MAINTAINS SUBROUTINE NAMING CONVENTIONS.
C
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *SMDEC(IHEAD,IERR)*
C         *CALL* *SMINT(IHEAD,IERR)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
C
                   ENTRY BULLSI ( IERR )
                   ENTRY BULLSN ( IERR )
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
      INCLUDE 'comindx.h'
      INCLUDE 'comstat.h'
C     ------------------------------------------------------------------
C
C*          1.   SET FLAGS AND WORKING POINTERS.
C                -------------------------------
 100  CONTINUE
C
C*    SHIP REPORTS IN OLD CODE ARE NOT HANDLED BY THIS ROUTINE SO
C     CHECK FOR MIMIMJMJ OF NNXX .
C
      IF ( KCHAR(IMI).EQ.78.AND.KCHAR(IMI+1).EQ.78) RETURN
C
C-----PRINT INPUT BULLETINS OF SURFACE REPORTS
C      CALL PRTBULL (1,IGS)
C-----PRINT INPUT BULLETINS OF SURFACE REPORTS
C
C     WORKING POINTER SET TO POINT TO FIRST LETTER OF MIMIMJMJ
C     GROUP.
C
      IPT = IMI
C
C     CLEAR FLAG WHICH INDICATES YYGGIW GROUP PRESENT.
C
      ISYYGG = 0
C
C     CLEAR FLAG WHICH INDICATES BULLETIN HEADER ALREADY WRITTEN
C     TO ERROR FILE.
C
      IHEAD = 0
C
C*    HANDLE MIMIMJMJ LINE . SET DATA BASE REPORT TYPE INDICATORS
C     IN DECODED REPORT HEADER  AND LOCATE START OF FIRST REPORT .
C
C     DATA BASE INDICATOR FOR LAND REPORTS IS 11 AND FOR SEA REPORTS
C     21 . THESE ARE MODIFIED LATER IF REDUCED SHIP , BUOY OR
C     AUTOMATIC REPORT ENCOUNTERED.
C
C
C     DISTINGUISH BETWEEN LAND ( TT = AA ) AND SEA ( TT = BB ) STATIONS.
C     'A' = 65 , 'B' = 66 .
C
C
C     MIMIMJMJ OF A--- OR -A-- ACCEPTED AS LAND STATION BULLETIN.
C     THE LINE LENGTH IS CHECKED IN CASE OF MISSING MIMIMJ LINE IN
C     A BULLETIN OF SHIP REPORTS AND SHIP'S NAME INCLUDES -A OR A- .
C
      L = JMI - IMI
      IF ( KCHAR(IMI).EQ.65.OR.KCHAR(IMI+1).EQ.65.AND.L.LT.15 )
     C THEN
C
C          SET DATA BASE CODE TYPE INDICATOR AND YYGGIW FLAG.
C
           KDEC(4) = 11
           ISYYGG = 1
C
C          LOCATE YYGGIW GROUP - NEXT FIGURE.
C
           CALL NEXTFIG ( IPT,JMI )
C
       elseif(KCHAR(IMI).EQ.79.OR.KCHAR(IMI+1).EQ.79) then
c           do not process mobile synop land in this stream
            RETURN
       ELSE
C
C          IF MIMIMJ IS B--- OR -B-- BULLETIN IS OF SEA REPORTS
C
           IF ( KCHAR(IMI).EQ.66.OR.KCHAR(IMI+1).EQ.66 )
     C         THEN
C
C                  do not process synop ship in this stream

                   RETURN

C                  SET DATA BASE CODE FIGURE FOR REPORT TYPE
C
C                  KDEC(4) = 21
C
C                  LOCATE D---D GROUP . THIS SHOULD BE THE NEXT
C                  CHARACTER AND ON A NEW LINE , BUT THERE ARE SOME
C                  COMMON VARIATIONS .
C
C                  (1) A YYGGIW OR YYGG GROUP IS ADDED AFTER BBXX ,
C                      ON THE SAME LINE .
C                  (2) THE REPORT CONTINUES ON THE SAME LINE AS BBXX.
C
C                  SET K1 TO POINT TO THE NEXT 'SPACE' CHARACTER (32)
C
C                  K1 = IPT
C                  CALL NEXTVAL ( K1,32,IGS )
C
C                  SET K2 TO POINT TO THE NEXT 'CR' CHARACTER.
C
C                  K2 = IPT
C                  CALL NEXTEND ( K2,IGS )
C
C                  IF A 'CR' CHARACTER IS ENCOUNTERED BEFORE 'SPACE'
C                  D---D IS ON THE NEXT LINE.
C
C                  IF ( K2.LT.K1 )
C    C                 THEN
C                          IPT = K2
C                      ELSE
C
C                          IF THE REST OF BBXX LINE CONSISTS OF ONLY
C                          5 FIGURES IT IS CONSIDERED TO BE YYGGIW,
C                          AND D---D IS SOUGHT ON THE NEXT LINE.
C
C                          K = K2 - K1
C                          IF ( K.GT.6 ) THEN
C                                            IPT = K1
C                                        ELSE
C                                            IPT = K2
C                                        END IF
C                      END IF
C
C                  IPT NOW POINTS TO THE CHARACTER BEFORE D---D GROUP.
C
C                  CALL NEXTPRT ( IPT,IGS )
C
               ELSE
C
C                  BULLETIN CANNOT BE IDENTIFIED FROM MIMIMJMJ , SO
C                  INSPECT  AA  OF ABBREVIATED HEADING.
C
C----              PRINT BULLETINS WITH ERROR IN MIMIMJMJ
C----
C----              CALL PRTBULL (1,IGS)
C----
C----              PRINT BULLETINS WITH ERROR IN MIMIMJMJ
C
C
C                  IF A1 IS V OR W AND A2 IS A,B,C,D,E,F,J OR X
C                  THEN BULLETIN IS OF SEA REPORTS.
C
                   IF ( (KCHAR(IAH+2).EQ.86.OR.KCHAR(IAH+2).EQ.87).
     C                  AND.(KCHAR(IAH+3).EQ.65.OR.KCHAR(IAH+3).
     C                  EQ.66.OR.KCHAR(IAH+3).EQ.67.OR.KCHAR(IAH+3).
     C                  EQ.68.OR.KCHAR(IAH+3).EQ.69.OR.KCHAR(IAH+3).
     C                  EQ.70.OR.KCHAR(IAH+3).EQ.74.OR.KCHAR(IAH+3).
     C                  EQ.88) )
     C                       THEN
C
C                                 SEA STATION
C
C                                 SET DATA BASE CODE FIGURE
C
                                  KDEC(4) = 21
C
C                                 CHECK MIMIMJ LINE LENGTH TO DECIDE
C                                 WHETHER LINE IS MISSING OR MIMIMJMJ
C                                 IS CORRUPT.
C
                                  IF ( (JMI-IMI).LT.11 )

     C                                   THEN
C
C                                            CORRUPT MIMIMJ
C
                                             CALL NEXTEND ( IPT,IGS )
                                             CALL NEXTPRT ( IPT,IGS )
                                         ELSE
C
C                                            MISSING MIMIMJMJ LINE
C
                                             IPT = IMI
                                         END IF
C
                              ELSE
C
C                                 LAND STATION BULLETIN
C
C                                 SET DATA BASE CODE FIGURE
C
                                  KDEC(4) = 11
C
C                                 CHECK MIMIMJMJ LINE LENGTH TO DECIDE
C                                 WHETHER GROUP IS CORRUPT OR LINE IS
C                                 MISSING.
C
                                  IF ( (JMI-IMI).LT.11 )
     C                                    THEN
C
C                                              CORRUPT MIMIMJMJ
C
                                               CALL NEXTEND (IPT,IGS)
                                               IPT =IPT - 5
C
C                                              SET YYGGIW FLAG
C
                                               ISYYGG = 1
                                          ELSE
C
C                                               MISSING LINE
C
                                                IPT = IMI
C
C                                               SET WORDS FOR YY GG IW
C                                               TO MISSING DATA VALUE.
C
                                                KINT(1) = MINDIC
                                                KINT(2) = MINDIC
                                                KINT(3) = MINDIC
C
                                          END IF
                    END IF
                END IF
        END IF
C
C
C
C
C
C
C
C     IF LAND STATION REPORT EXTRACT 'YYGGIW' FROM 'MIMIMJMJ' LINE.
C
C
  101 IF ( KDEC(4).EQ.11.AND.ISYYGG.EQ.1 )
     C    THEN
              CALL NEXTPRT ( IPT,IGS )
              CALL EXTGRP ( IPT,2,2,1,0,0,1,IRET)
              CALL NEXTPRT ( IPT,IGS )
              ISYYGG = 0
C
C             CHECK VALUES OF YY,GG AND IW . IF ANY ERROR IS
C             FOUND DECODED VALUE IS CHANGED TO MISSING DATA VALUE
C             AND GROUP FLAGGED AS BEING IN ERROR . ERRORS IN THIS
C             GROUP ARE NOT FATAL AS YYGG FROM HEADING AND IW FROM
C             WMO STATIONS MASTER FILE ARE SUBSTITUTED LATER , IF
C             NECESSARY.
C
              I = 0
              IF (KINT(1).LT.1.OR.KINT(1).GT.31)
     C             THEN
                       I = 1
                       KINT(1) = MINDIC
                   END IF
C
              IF ( KINT(2).LT.0.OR.KINT(2).GT.23)
     C             THEN
                       I = 1
                       KINT(2) = MINDIC
                   END IF
C
              IF ( KINT(3).EQ.2.OR.KINT(3).LT.0.OR.KINT(3).GT.4)
     C             THEN
                       I = 1
                       KINT(3) = MINDIC
                   END IF
C
              IF ( I.NE.0 )
     C             THEN
                       KCHAR(IPT-1) = IOR(KCHAR(IPT-1),128)
                       NOER(1,3)=NOER(1,3)+1
                   END IF
           END IF
C
C
C***
C*    LOCATE END OF REPORT ( = ) AND INCREMENT COUNTER.
C***
C
      IEQ = IPT
      CALL NEXTEQ ( IEQ,IGS )
C
C
      NUMREP(1) = NUMREP(1) + 1
C
C
C
C***
C*    DECODE REPORT .
C***
C
C     CONVERT REPORT TO INTERMEDIATE FORMAT.
C
      CALL SMINT( IHEAD,IERR )
C
C
C     "NIL" REPORTS (KERR=1) WILL NOT BE DECODED FOR
C     DATA MONITORING PURPOSES
C
C     IF(KERR .NE. 0) GO TO 200
C
C
C     CONVERT INTERMEDIATE TO DECODED FORMAT AND WRITE
C     TO FILE.
C
      CALL SMDEC ( IHEAD,IERR )
C
      IF ( KERR.NE.0 ) GO TO 200
C
C
C
C
C***
C*    LOCATE START OF NEXT REPORT.
C***
C
200   IPT = IEQ + 1
      CALL NEXTPRT ( IPT,IGS )
C
C***
C*    CHECK FOR END OF BULLETIN.
C***
C
      IF ( IPT.GT.IGS ) THEN
C
C                           END OF BULLETIN , SO IF ANY REPORT HAS
C                           BEEN WRITTEN TO ERROR FILE , ADD 'GS'
C                           CHARACTER BEFORE RETURNING.
C
                            IF (IHEAD.EQ.0) THEN
C                               Create BUFR
                                olast=.true.
                                CALL SYNEXP1(olast,IERR)
                                RETURN
                            else
C                               Create BUFR
                                olast=.true.
                                CALL SYNEXP1(olast,IERR)
                                RETURN
                            END IF
                            IHEAD = 2
                            CALL SAVREP ( IHEAD,IERR )
C
C-----                      PRINT SURFACE DATA WRITTEN TO ERROR FILE
C-----
C-----                      CALL PRTBULL (1,IGS)
C-----
C-----                      PRINT SURFACE DATA WRITTEN TO ERROR FILE
C
                            RETURN
                        END IF
C
C
C***
C*    RESET LAND OR SEA STATION RDB CODE FIGURE.
C***
C
      IF ( KDEC(4).GT.14 ) THEN
                               KDEC(4) = 21
                           ELSE
                               KDEC(4) = 11
                           END IF
C
C***
C*    IF LAND STATION REPORT IT IS POSSIBLE TO GET NEW 'MIMIMJMJ'
C*    AND 'YYGGIW' GROUPS , SO CHECK .
C***
C
      IF ( KDEC(4).EQ.11.AND.KCHAR(IPT).EQ.65 )
     C                 THEN
C
C                          SET YYGGIW FLAG AND LOCATE START OF NEW
C                          YYGGIW GROUP.
C
                           ISYYGG = 1
                           CALL NEXTFIG ( IPT,IGS )
                           IF ( IPT.GE.IGS ) RETURN
                       END IF
      GO TO 101
C
C
      END
      SUBROUTINE SMDEC ( IHEAD,IERR )
C
C
C**** *SMDEC*
C
C
C     PURPOSE.
C     --------
C
C         DECODE SURFACE REPORT FROM INTERMEDIATE FORMAT
C         TO DECODED SURFACE REPORT
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SMDEC(IHEAD,IERR)*
C
C          INPUT    :  REPORT IN INTERMEDIATE FORMAT IN KINT
C
C          OUTPUT   :  DECODED REPORT ON KDEC
C
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *IC3333 (     )*      FOR LATTITUDE AND LONGITUDE
C         *CALL* *IT5TODC(     )*      CONVERSION FROM CITT5 TO DISPLAY CODE
C         *CALL* *IC0877 (     )*      WIND DIRECTION AND SPEED
C         *CALL* *IC4377 (     )*      HORIZONTAL VOISIBILITY
C         *CALL* *IC3845 (     )*      TEMPERATURE
C         *CALL* *IC0264 (     )*      INDICATOR OF ISOBARIC SURFACE  (A3)
C         *CALL* *IC3590 (     )*      PRECIPITATION
C         *CALL* *ICTRTR (     )*      PERIOD FOR PRECIPITATION MEASUREMENT
C         *CALL* *IC1677 (     )*      HEIGHT OF BASE OF CLOUD  (HH)
C         *CALL* *IC0700 (     )*      SHIP'S DIRECTION
C         *CALL* *IC4451 (     )*      SHIP'S SPEED
C         *CALL* *ICPWPW (     )*      PERIOD OF WAVES
C         *CALL* *ICHWHW (     )*      HEIGHT OF WAVES
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C         *CALL*         (     )*
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
      INCLUDE 'comindx.h'
C
C     ------------------------------------------------------------------
C           1. IN THIS SUBROUTINE THE ONLY PARTS OF THE HEADER
C              DECODED ARE IDENTIFIER , THE LENGTH OF REPORT ,
C              DAY OF THE MONTH AND TIME (HOUR & MIN) FROM REPORT
C              AND IN CASE OF SHIP OR BUOY
C              LAT, LONG, ALT=0
C
 100  CONTINUE
C
C
C     CLEAR ERROR INDICATOR.
C
      IF(KERR.GT.1) RETURN
C
      KERR = 0
      IERR = 0 
C
C*    SET DECODED REPORT TO MISSING DATA VALUE
C
      DO 1 I=25,200
      KDEC(I)=MINDIC
 1    CONTINUE
C
C
C
C*    CHECK IF SHIP OR BUOY
C
      IF(KDEC(4) .LT. 21 ) GO TO 101
C
C
C
C*   LAT & LONG
C
C
      CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,KDEC(5),KDEC(6))
C
C
C
C
C*    ALTIT   FOR SEA STATION=0
C
C
      KDEC(8)=0
C
C
C
C
101   CONTINUE
C
C*    IDENT   STATION OR BUOY IDENTIFICATION NUMBER
C             OR SHIP CALL SIGN
C             KINT(12) - KINT(16) CONTAIN ID.
C
      KDEC(7)=88
C
C
C*     DAY AND TIME
C
C      CHECK IF DAY AND HOUR ARE MISSING. IN THAT CASE USE 
C      DATE FROM ABBREVIATED HEADER.
C
       KDEC(1)=KINT(1)
       KDEC(2)=KINT(2)
C
       IF(KINT(1).EQ.MINDIC) THEN
                                KDEC(1)=KDEC(10)
                                IF(KDEC(10).EQ.MINDIC) RETURN
                             END IF
C
       IF(KINT(2).EQ.MINDIC) THEN
                                KDEC(2)=KDEC(11)
                                IF(KDEC(11).EQ.MINDIC) RETURN
                             END IF
C
      KDEC(9)=0
C
C
C*    REPORT LENGTH
C
C
      KDEC(24)=120
C
C
      IF(KDEC(20).EQ.1) THEN
C       NILL report
        GO TO 950
      END IF

C
C
C*    SECTION 1
C
C
C*    DD & FF   WIND DIRECTION AND SPEED
C
C
      K=MINDIC
      IF(KDEC(4) .GE. 21) GO TO 120
      K=KDEC(17)*1000 + KDEC(16)
120   CONTINUE
C
      if(kint(255).eq.0.and.kint(256).ne.mindic) then
         CALL IC0877(KINT(19),KINT(256),KINT(3),K,MINDIC,KDEC(25),
     1            KDEC(26))
      else
         CALL IC0877(KINT(19),KINT(20),KINT(3),K,MINDIC,KDEC(25),
     1            KDEC(26))
      end if
C
C
C*    VV   HORIZONTAL VISIBILITY
C
C
      CALL IC4377(KINT(17),MINDIC,KDEC(27))
C
C
C
C
C*    WW, W1, W2   PRESENT AND PAST WEATHER
C
C
      IF(KINT(38) .EQ. MINDIC .AND. (KINT(15) .EQ. 2 .OR.
     *                 KINT(15) .EQ. 5))
     *                     THEN
                              KDEC(28)=2
                              KDEC(29)=1
                              KDEC(30)=1
                              GO TO 210
                           END IF
C
C
      DO 200 I=39,41
      IF(KINT(I) .EQ. MINDIC) GO TO 200
      KDEC(I-11)=KINT(I)
200   CONTINUE
C
210   CONTINUE
C
C
C*    TTT   AIR TEMPERATURE IN TENTHS OF DEGREE CELSIUS
C
C
C
      CALL IC3845(KINT(23),KINT(22),KDEC(16),KDEC(17),MINDIC,KDEC(31))
C
C
C
C*    TDTDTD   DEW POINT TEMPERATURE IN TENTHS OF DEGREE
C
C
C
      IF(KINT(26) .EQ. MINDIC) GO TO 320
      CALL IC3845(KINT(26),KINT(25),KDEC(16),KDEC(17),MINDIC,KDEC(32))
C
C
C
C***  UUU RELATIVE HUMIDITY
C
C
C     IF (KINT(25) .EQ. 9) THEN DEW POINT TEMP IS NOT AVAILABLE
C     AND RELATIVE HUMIDITY IS MEASURED INSTEAD
C
      IF(KINT(25) .EQ. 9) KDEC(33)=KINT(26)
C
C     CHECK RANGE
C
      IF ( KDEC(33).GT.100 ) KDEC(33) = MINDIC
C
320   CONTINUE
C
C
C
C*    P0P0P0   PRESSURE AT STATION LEVEL IN TENTHS OF HECTOPASCAL
C
C
      IF(KINT(28) .EQ. MINDIC) GO TO 340
      KDEC(34)=KINT(28)
      KK = KINT(28) / 1000
      IF ( KK.EQ.0 ) KDEC(34) = KDEC(34) + 10000
340   CONTINUE
C
C
C      CHECK IF THERE IS SEA LEVEL PRESSURE OR GEOPOTENTIAL IN THIS
C      THIS REPORT (IF A3 (=KINT(30)) .NE. 0 AND .NE. 9, THEN GEOPOT
C
C
      IF(KINT(30).EQ.MINDIC.OR.KINT(31).EQ.MINDIC ) GO TO 370
      IF(KINT(30) .NE. 0 .AND. KINT(30) .NE. 9) GO TO 350
C
C
C
C*    PRESSURE AT SEA LEVEL IN TENTHS OF HECTOPASCAL
C
C
      KDEC(35)=KINT(31) + 1000*KINT(30)
      IF(KINT(30) .EQ. 0) KDEC(35)=KINT(31) + 10000
      GO TO 370
C
C
C
350   CONTINUE
C
C
C***  A3   INDICATOR OF STANDARD ISOBARIC SURFACE (CODE 264)
C
C
C
      CALL IC0264(KINT(30),MINDIC,KDEC(36))
C
C
C*    HHH   GEOPOTENTIAL AT AN AGREED ISOBARIC SURF, GIVEN BY A3
C
C
      KDEC(37)=KINT(31)
C
C     ADD 1000 IF 850 HPA LEVEL
C
      IF (KINT(30).EQ.8) KDEC(37)=KDEC(37) + 1000
C
C     ADD 2000 OR 3000 IF 700 HPA LEVEL
C
      IF (KINT(30).EQ.7) THEN
                             IF (KINT(31).LT.500)KDEC(37)=KDEC(37)+3000
                             IF (KINT(31).GE.500)KDEC(37)=KDEC(37)+2000
                         END IF
C
C     ADD 5000 IF 500 HPA LEVEL
C
      IF (KINT(30).EQ.5) KDEC(37) = KDEC(37) + 5000
C
C
C
370   CONTINUE
C
C*    A   CHARACTERISTIC OF PRESSURE TENDENCY DURING 3 HOURS
C
C
      IF(KINT(33) .NE. MINDIC .AND. KINT(33).NE.9) THEN
         KDEC(38)=KINT(33)
      ELSE
         KDEC(38)=MINDIC
      END IF
C
C
C
C*    PPP   AMOUNT OF PRESSURE TENDENCY IN TENTHS OF HECTOPASCAL
C
C
      IF ( KINT(34).NE.MINDIC.AND.KINT(33).NE.9 )
     C   THEN
             KDEC(39)=KINT(34)
C
C            VALUE IS NEGATIVE IF CHARACTERISTIC ( A ) IS 5 - 8.
C
             IF (KDEC(38).GE.5.AND.KDEC(38).LE.8) KDEC(39) = -KDEC(39)
C
         END IF
C
C
C*     TEST IF PRESSURE TENDENCY IS FOR 24 HOUR PERIOD
C      INDICATED BY KINT(104)
C
c     IF(KINT(104) .EQ. 8 .OR. KINT(104) .EQ. 9)
c    1                   THEN
c                           KDEC(38)=9
c                           KDEC(39)=KINT(105)
c                        END IF
C
c     IF(KINT(104) .EQ. 9 .AND. KINT(105) .NE. MINDIC)
c    1   KDEC(39)=-KDEC(39)
C
C
C
C***  RRR   AMOUNT OF PRECIPITATION TENTHS OF MILLIMETRES
C
C
      CALL IC3590(KINT(36),KINT(14),MINDIC,KDEC(40))
C
C     DURATION NOT DECODED IF RAINFALL VALUE IS MISSING.
C
      IF (KDEC(40).EQ.MINDIC) GO TO 415
C
C
C
C
C***  TR   DURATION OF PERIOD OF REFERENCE FOR PRECIP.(HOURS)
C
      IHOURS=KDEC(2)
      IF(IHOURS .EQ. MINDIC) IHOURS=KDEC(11)
      IF(IHOURS .LT. 0 .OR. IHOURS .GT. 24) IHOURS=MINDIC
      IF(IHOURS .EQ. MINDIC) GO TO 415
C
      CALL ICTRTR (KDEC(16),KDEC(17),KINT(37),IHOURS,MINDIC,KDEC(41))
C
C
415   CONTINUE
C
C
C***  N   TOTAL CLOUD COVER
C
C
      IF(KINT(18) .EQ. MINDIC) THEN
         KDEC(42)=MINDIC
      ELSE
         KDEC(42)=KINT(18)
      END IF
C
C
C
C***  H   HEIGHT OF THE BASE OF LOWEST CLOUD
C
C
      IF(KINT(16) .EQ. MINDIC) THEN
                                  IF (KDEC(4).NE.23)
     C                            KDEC(43)=16382
                                  GO TO 430
                               END IF
C
      IF(KINT(18) .EQ. 0) THEN
                             KDEC(43)=16381
                             GO TO 430
                          END IF
C
      LOWEST=MINDIC
C
C     loop for different cloud types
C
      DO 425 I=46,44,-1
      IF(KINT(I) .EQ. MINDIC) GO TO 425
      IF (KINT(I).EQ.0) GO TO 425
      LOWEST=I-44
425   CONTINUE
C
C
C
      CALL IC1600(KINT(16),LOWEST,MINDIC,KDEC(43))
C
430   CONTINUE
C
C
C
C***  NH   TOTAL LOW CLOUDS
C
C
      IF(KINT(42) .EQ. MINDIC) GO TO 451
      KDEC(44)=KINT(43)
C
C
C
C***  CL, CM, CH   TYPE LOW MEDIUM AND HIGH CLOUDS
C
C
      DO 450 I=45,47
      KDEC(I)=KINT(I-1)
      IF(KINT(I-1) .EQ. MINDIC) KDEC(I)=14
450   CONTINUE
C
451   CONTINUE
C
C***  HH   HEIGHT OF THE BASE OF THE LOWEST CLOUD
C
C      IF(KINT(48) .EQ. MINDIC) GO TO 455
C
C      CALL IC1677(KINT(48),MINDIC,KDEC(43))
C
455   CONTINUE
C
C
C***
C*
C*           SECTION 2
C*
C*    CHECK IF THERE IS SECTION 2 IN THIS REPORT
C***
C
      IF(KINT(50) .EQ. MINDIC) GO TO 690
C
C
C***  DS   DIRECTION OF THE SHIP
C
C
      CALL IC0700(KINT(51),MINDIC,KDEC(49))
C
C
C
C***  VS   SHIP"S SPEED
C
C
      CALL IC4451(KINT(52),MINDIC,KDEC(50))
C
C
C
C***  TWTWTW   SEA-SURFACE TEMPERATURE
C
C
      IF(KINT(53) .EQ. MINDIC) GO TO 510
      IF(KINT(54) .EQ. 0) KDEC(51)=KINT(55)
      IF(KINT(54) .EQ. 1) KDEC(51)=-1*KINT(55)
510   CONTINUE
C
C
C
C***  PWAPWA   PERIOD OF WAVES IN SECONDS
C
C
      CALL ICPWPW(KINT(57),MINDIC,KDEC(52))
C
C
C
C***  HWAHWA   HEIGHT OF WAVES IN DECIMETERS 
C
C
      CALL ICHWHW(KINT(58),MINDIC,KDEC(53))
C
C       CHECK IF HIGHT WITH .1 M REPORTED.
C
      IF(KINT(250).EQ.70) THEN
         IF(KINT(251).NE.MINDIC) KDEC(53)=KINT(251)
      END IF
C
C
C
C***  PWPW   PERIOD OF WIND WAVES IN SECONDS
C
C
      CALL ICPWPW(KINT(60),MINDIC,KDEC(54))
C
C
C
C
C***  HWHW   HEIGHT OF WIND WAVES IN DECIMETERS 
C
C
      CALL ICHWHW(KINT(61),MINDIC,KDEC(55))
C
C
C
C***  DW1DW1 & DW2DW2   DIRECTION FROM WHICH WAVES ARE COMING
C
C
      DO 560 I=56,57
      IF(KINT(I+7) .EQ. MINDIC) GO TO 560
      KDEC(I)=KINT(I+7)*10
      IF(KINT(I+7) .EQ. 99) KDEC(I)=0
560   CONTINUE
C
C
C
C***  PW1PW1, HW1HW1    PERIOD AND HEIGHT OF
C***  PW2PW2, HW2HW2    SWELL WAVES
C
C
      CALL ICPWPW(KINT(66),MINDIC,KDEC(58))
      CALL ICHWHW(KINT(67),MINDIC,KDEC(59))
C
      CALL ICPWPW(KINT(69),MINDIC,KDEC(60))
      CALL ICHWHW(KINT(70),MINDIC,KDEC(61))
C
C
C
C
C***  IS     TYPE OF ICE
C***  ESES   ICE THICKNESS IN DECIMETERS
C***  RS     RATE OF ICING
C
C
      DO 620 I=72,74
      IF(KINT(I) .EQ. MINDIC) GO TO 620
      KDEC(I-10)=KINT(I)
620   CONTINUE
C
      IF(KDEC(63) .NE. MINDIC) KDEC(63)= (KDEC(63) + 5) / 10
C
C
C***  CI    CONCENTRATION OR ARRANGEMENT OF SEA ICE
C***  SI    STAGE OF DEVELOPMENT
C***  BI    LAND ICE
C***  DI    BEARING OF ICE
C***  ZI    TREND
C
C
      DO 650 I=76,80
      IF(KINT(I) .EQ. MINDIC) GO TO 650
      KDEC(I-11)=KINT(I)
650   CONTINUE
C
      IF(KDEC(68) .EQ. 99) KDEC(68)=0
C
C***         Wet bulb temperature (from 2.11.1993)
C
      if(kint(252).ne.mindic.and.kint(253).ne.mindic.and.
     1   kint(254).ne.mindic) then
         iwtsign=1
         if(kint(253).eq.0) iwtsign=1
         if(kint(253).eq.1) iwtsign=-1
         if(kint(253).eq.2) iwtsign=-1
         if(kint(253).eq.5) iwtsign=1
         if(kint(253).eq.6) iwtsign=-1
         if(kint(253).eq.7) iwtsign=-1
         kdec(120)=iwtsign*kint(254)
      end if
c
690   CONTINUE
C
C***
C*
C*         SECTION 3
C*
C*     THIS IS THE REGIONAL PART AND THERE ARE DIFFERENCIES
C*     IN DIFFERENT REGIONS. REGION NUMBER IS KDEC(17).
C***
C
C***
C*      TEST IF THERE ARE ANY REGIONAL DATA
C***
C
      IF(KDEC(17) .EQ. MINDIC) GO TO 950
      IF(KINT(81) .EQ. MINDIC) GO TO 950
C
C
C***   TGTG   GROUND MIN TEMPERATURE IN TENTHS OF DEGREE
C             ONLY REGION I
C
C
C
C     THE FIRST GROUP IS 0TGTGRCRT AT 0600Z OR 0//RCRT
C     AT 0000 OR 1200. ONLY TGTG IS DECODED. TIME IS KDEC(11).
C
      IF(KDEC(11) .NE. 6) GO TO 700
      IF(KINT(83) .EQ. MINDIC) GO TO 700
      IF(KDEC(17) .NE. 1) GO TO 700
C
      KDEC(70)=KINT(83)*10
      IF(KDEC(70) .GT. 500) KDEC(70)=500-KDEC(70)
C
700   CONTINUE
C     Ground minimum Temperature
      IF(KDEC(17) .NE. 1) THEN
         IF(KINT(96).NE.MINDIC) then
            KDEC(70)=KINT(96)
            IF(KINT(95).EQ.1) KDEC(70)=-KDEC(70)
         END IF
      END IF
C
C
C***   TXTXTX   MAX AIR TEMPERATURE IN TENTHS OF DEGREE
C               THE PERIOD FOR MAX IS DIFFERENT FOR
C               DIFFERENT REGIONS
C
C
C     FOR REGION IV AT 1200Z THE MAX TEMP IS DEFINED FOR THE PERIOD
C     OF CALENDAR DAY, AND IT WILL NOT BE DECODED.
C     FOR REGION III DAYTIME TEMPERATURE IS USED, AND IT WILL
C     NOT BE DECODED EITHER.
C
      IF(KDEC(17) .EQ. 4 .AND. KDEC(11) .EQ. 12) GO TO 720
      IF(KDEC(17) .EQ. 3) GO TO 720
C
C
      IF(KINT(87) .EQ. MINDIC) GO TO 720
      CALL IC3845(KINT(89),KINT(88),KDEC(16),KDEC(17),MINDIC,KDEC(71))
C
C
C***   TX-PERIOD   LENGTH OF THE PERIOD FOR MAX TEMPERATURE (HOURS)
C                  IT VARIES FROM REGION TO REGION
C            REGION I   12 HOURS  (AT 1800)
C            REGION II  12 HOURS  (AT 1800)
C            REGION III DAYTIME TEMPERATURE  (MINDIC)
C            REGION IV  12 HOURS  (AT 0000 & 1800)
C                       24 HOURS  (AT 0600)
C                       CALENDAR DAY (AT 1200)   (MINDIC)
C            REGION V   24 HOURS  (AT 1200)
C            REGION VI  12 HOURS  (AT 1800)
C
      IF(KDEC(17) .EQ. 1 .OR. KDEC(17) .EQ. 2 .OR. KDEC(17) .EQ. 6)
     1                    THEN
                              KDEC(72)=12
                              GO TO 720
                          END IF
C
C
      IF(KDEC(17) .EQ. 4)
     1       THEN
                 IF(IHOURS .EQ. 12) GO TO 720
                 IF(IHOURS .EQ. 0 .OR. IHOURS .EQ. 18)
     1                    THEN
                              KDEC(72)=12
                              GO TO 720
                          END IF
                 IF(IHOURS .EQ. 6)
     1                    THEN
                              KDEC(72)=24
                              GO TO 720
                          END IF
             END IF
C
      IF(KDEC(17) .EQ. 5) KDEC(72)=24
C
720   CONTINUE
C
C
C***   TNTNTN   MIN AIR TEMPERATURE IN TENTHS OF DEGREE,
C               THE PERIOD FOR MIN VARIES FROM REGION TO REGION
C
C      FOR REGION III MIN TEMP IS DEFINED AT NIGHT TIME,
C      AND IT WILL NOT BE DECODED
C
      IF(KDEC(17) .EQ. 3) GO TO 740
C
C
      IF(KINT(90) .EQ. MINDIC) GO TO 740
      CALL IC3845(KINT(92),KINT(91),KDEC(16),KDEC(17),MINDIC,KDEC(73))
C
C
C
C***   TN-PERIOD   LENGHT OF THE PERIOD FOR MIN TEMPERATURE
C                  REGION I    12 HOURS  (AT 0600)
C                         II   12 HOURS  (AT 0600)
C                         III  NIGHT TIME  (MINDIC)
C                         IV   18 HOURS  (AT 0000)
C                         V    24 HOURS  (AT 0000)
C                         VI   12 HOURS  (AT 0600)
C
      IF(KDEC(17) .EQ. 1 .OR. KDEC(17) .EQ. 2 .OR.
     1   KDEC(17) .EQ. 6) THEN
                             KDEC(74)=12
                             GO TO 740
                          END IF
C
      IF(KDEC(17) .EQ. 4) THEN
                             IF(IHOURS .EQ. 0)
     1                             THEN
                                      KDEC(74)=18
                                      GO TO 740
                                   END IF
                             IF(IHOURS .EQ. 6 .OR. IHOURS .EQ.
     1                         18) THEN
                                      KDEC(74)=24
                                      GO TO 740
                                   END IF
                             IF(IHOURS .EQ. 12)
     1                             THEN
                                      KDEC(74)=12
                                      GO TO 740
                                   END IF
                          END IF
C
      IF(KDEC(17) .EQ. 5) KDEC(74)=24
C
740   CONTINUE
C
C***  RRR AMOUNT OF PRECIPITATION 1/10THS OF MM
C
C     DECODE RAINFALL IF NOT ALREADY DECODED FROM SECTION 1.
C
C     IF (KDEC(40).NE.MINDIC) GO TO 750
C
      CALL IC3590 (KINT(114),KINT(14),MINDIC,KDEC(140))
C
C     DURATION NOT DECODED IF RAINFALL VALUE IS MISSING.
C
C     IF (KDEC(40).EQ.MINDIC) GO TO 750
C
C
C***DURATION OF RRR
         IHOURS = KDEC(2)
         IF (IHOURS.EQ.MINDIC) IHOURS=KDEC(11)
         IF (IHOURS.LT.0.OR.IHOURS.GT.24) IHOURS=MINDIC
         IF (IHOURS.EQ.MINDIC) GO TO 750
C
C
      CALL ICTRTR (KDEC(16),KDEC(17),KINT(115),IHOURS,MINDIC,KDEC(141))
C
C
  750 CONTINUE
C
C***   NS     AMOUNT OF CLOUD  (CODE FIGURE)
C      C      TYPE OF CLOUD
C      HSHS   HEIGHT OF BASE OF CLOUD LAYER
C
C
      DO 800 I=75,84,3
      J=(I - 75)/3 + I
      IF(KINT(J+46) .EQ. MINDIC) GO TO 810
      KDEC(I)=KINT(J+47)
      KDEC(I+1)=KINT(J+48)
      CALL IC1677(KINT(J+49),MINDIC,KDEC(I+2))
800   CONTINUE
C
810   CONTINUE
C
C
C      KDEC(87) -- KDEC(94)   SPSP SPSP  SPECIAL PHENOMENA
C                                        (CODE FIGURES)
C
      DO 900 I=87,94
      J=(I-87)/2 + I
      IF(KINT(J+50) .EQ. MINDIC) GO TO 910
      KDEC(I)=KINT(J+51)
      KDEC(I+1)=KINT(J+52)
900   CONTINUE
C
910   CONTINUE
C
C
C***   E   STATE OF GROUND, NO SNOW OR ICE
C
      IF(KINT(93) .NE. MINDIC) KDEC(97)=KINT(94)
C
C
C***   E'   STATE OF GROUND WITH SNOW OR ICE
C
C
      IF(KINT(98) .NE. MINDIC)
     *            THEN
                     KDEC(98)=KINT(99)
                     KDEC(99)=KINT(100)
                     IF(KDEC(99) .EQ. 997) KDEC(99)=0
                     IF(KDEC(99) .GE. 998) KDEC(99)=MINDIC
                     IF(KDEC(99) .GE. 999) KDEC(99)=MINDIC
                  END IF
C
C
C
C     FROM CHINA SNOW INFORMATION IS IN SPECIAL PHENOMENA. 9 SPSP SPSP
C     IN FORM 93 SPSPSP, WHERE SPSPSP = SNOW DEPTH IN CM.
C
      IF( KDEC(16) .EQ. 250 )
     C   THEN
            IF(KINT(137) .EQ. 9)
     C         THEN
                  INDSNOW = KINT(138) / 10
                  IF(INDSNOW .EQ. 3)
     C               THEN
                        KDEC(99) = 100 * (KINT(138) - 30) + KINT(139)
                        KDEC(98) = 1
                     END IF
               END IF
         END IF
C
C
C
C     IF E' IS MISSING AND E IS AVAILABLE IT MEANS THAT SNOWDEPTH=0
C
      IF((KDEC(98) .EQ. MINDIC) .AND. (KDEC(97) .NE. MINDIC))
     1    KDEC(99)=0
C
C     J1J2J3J4J5 GROUP
C
C      EEE EVAPORATION/EVAPOTRANSPIRATION
C
      IF(KINT(181).NE.MINDIC) THEN
C        KG/M**2
         IF(KINT(182).NE.MINDIC) KDEC(110)=KINT(182)/10.
      END IF
C
C      SSS DURATION OF SUNSHINE 
C
      IF(KINT(189).NE.MINDIC) THEN
         IF(KINT(191).NE.MINDIC) THEN
            IH=KINT(191)/10
            IMM=(KINT(191)-IH*10)*6
            KDEC(111)=IH*60+IMM
         END IF
      END IF
C
C       NET RADIATION OVER 24 HOUR PERIOD
C
C          (OTHER RADIATION DATA ARE NOT PASSED)!!!!
C        
      IF(KINT(192).NE.MINDIC.AND.KINT(193).NE.MINDIC) THEN
         KDEC(112)=KINT(193)
      END IF
      IF(KINT(194).NE.MINDIC.AND.KINT(195).NE.MINDIC) THEN
         KDEC(112)=-KINT(195)
      END IF
C
950   CONTINUE
C
C
C      CALL PRTKINT(KINT,1,300,MINDIC) 
C      CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
C
      olast=.false.
      CALL SYNEXP1(olast, IERR )
C     CALL QCSYNOP( 1,KDEC(4),KDEC(23),IERR )
C
C     CALL SYNEXP2( IERR )
C     CALL QCSYNOP( 2,KDEC(4),KDEC(23),IERR )
C
C
C---- PRINT OF INTERMEDIATE AND DECODED FORM OF REPORTS
      RETURN
C
C
      END
      SUBROUTINE SMINT (IHEAD,IERR)
C
C
C**** *SMINT*
C
C
C     PURPOSE.
C     --------
C
C         CONVERT SURFACE REPORTS FROM CCITT. NO.5 CHARACTER
C         FORMAT TO INTERMEDIATE ( INTEGER ) FORMAT.
C
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SMINT(IHEAD,IERR)*
C
C          INPUT     : REPORT IN KCHAR(IPT) - KCHAR(IEQ) , IN  CCITT 5 ,
C                      1 CHARACTER PER WORD.
C
C                      IHEAD = 0 INDICATES BULLETIN HEADER NOT ALREADY
C                                WRITTEN TO ERROR FILE.
C                            = 1 MEANS HEADER ALREADY WRITTEN TO ERROR FILE.
C
C                      IERR IS NOT USED ON INPUT.
C
C
C         OUTPUT     : REPORT IN INTEGER FORMAT IN ARRAY 'KINT' IN
C                      DESIRED FORMAT ( SEE SEPARATE DOCUMENTATION )
C
C                      IERR = 1 IF ANY FILE HANDLING ERROR ENCOUNTERED.
C
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *XXXXXXX(XXXX)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
      INCLUDE 'comindx.h'
      INCLUDE 'comstat.h'
c
      character*21 CTSTAMP
      character*4  CSTREAM
      character*256 cf
C
C     ------------------------------------------------------------------
C*          1. CLEAR FLAGS AND ERROR INDICATOR.KEEP POINTER
C              TO FIRST CHARACTER OF REPORT.
C
 100  CONTINUE
C
C     POINTER TO FIRST CHARACTER.
C
      KEEP = IPT
C
C     FLAG INDICATING FIRST DECODING ATTEMPT ON REPORT.
C
      IFIRST = 0
C
C     ERROR INDICATOR.
C
   10 KERR = 0
C
C
C***
C*    CHECK FOR 'NIL' REPORT.
C***
C
      LEN = IEQ - IPT
C
C     RETURN IF REPORT SHORTER THAN 5 CHARS, FLAG ERROR = 2
C     SO IT WONT BE DECODED AT ALL (EVEN AS 'NIL')
C
      IF(LEN .LT. 5)
     C   THEN
            KERR = 2
            RETURN
         END IF
C
      NIL = 0
C
      IF (KDEC(4).LE.14.AND.LEN.LE.17) NIL = 1
      IF (KDEC(4).GE.21.AND.LEN.LE.28) NIL = 1
C
      IF (NIL.EQ.1) THEN
                         KERR = 1
                         NOER(1,1) = NOER(1,1) + 1
C
C-----                   PRINT 'NIL' SURFACE REPORTS
C-----
C-----                   CALL PRTBULL (IPT,IEQ)
C-----
C-----                   PRINT 'NIL' SURFACE REPORTS
C
                    END IF
C
C
C***
C*    SET AREA FOR DECODED REPORT TO MISSING DATA VALUE.
C*    START IS IN WORD 4 AS YY GG IW FOR LAND STATIONS MAY ALREADY
C*    HAVE BEEN INSERTED .
C***
C
      DO 101 I=4,300
         KINT(I) = MINDIC
  101 CONTINUE
C
C
C     WORDS 9-13 SET TO CCITT.5 'XXX  ' ( 'X' = 88, 'SPACE' = 32 )
C     STATION/SHIP IDENTIFIER RETAINED IN THESE WORDS IN CCITT 5 CHARS.
C
      DO 200 I=9,11
         KINT(I) = 88
  200 CONTINUE
C
      KINT(12) = 32
      KINT(13) = 32
C
C***
C*    CLEAR DATA FOR PREVIOUS REPORT FROM DECODED REPORT HEADER AREA.
C***
C
      DO 300 I=1,3
         KDEC(I) = MINDIC
  300 CONTINUE
C
      DO 400 I=5,9
         KDEC(I) = MINDIC
  400 CONTINUE
C
C     CLEAR FLAG FIELDS , RETAINING 'COR' FLAG.
C
      KDEC(13) = 0
C
      KDEC(15) = IAND(KDEC(15),4)
C
      DO 500 I=16,17
         KDEC(I) = MINDIC
  500 CONTINUE
C
      KDEC(20)=0
C
      DO 600 I=22,23
         KDEC(I) = MINDIC
  600 CONTINUE
C
C
C***
C*    CONVERT SECTION 0 , WHICH IS DIFFERENT FOR LAND AND SEA
C*    STATIONS.
C***
C
  700 IF ( KDEC(4).EQ.11.OR.KDEC(4).EQ.14)
     C    THEN
C
C             LAND STATION .
C
C
C             RETAIN POINTER TO STATION IDENTIFIER .
C
C             IIIII
C
              KPT = IPT
              ID = 5
C
              CALL EXTINT ( IPT,5,4 )
              CALL LOCSTAT (IWIND,IRET)
C
              IF ( IRET.EQ.1 ) THEN
C
C                 STATION NUMBER IN RANGE BUT NO MASTER FILE ENTRY
C                 ,SO REPORT IS IGNORED.
C
                  KERR=2
                  NOER(1,2)=NOER(1,2)+1
C
C--               PRINT IIIII WITH NO MASTER FILE ENTRY
C--
c                 cf=' '
c                 cf='/home/ma/maa/feed/err/unknown_synop_station.dat'
c                 icf=index(cf,' ')
c                 icf=icf-1
c
c                 OPEN(UNIT=55,
c    1                FILE=cf(1:icf),
c    2                ACCESS='APPEND',
c    4                FORM='FORMATTED',
c    5                STATUS='UNKNOWN'        )
C
c                 WRITE (55,9901) KINT(4)
c                 CLOSE(55)
                  WRITE (*,9901) KINT(4)
 9901             FORMAT (1H ,'NO MASTER FILE ENTRY - ',I5.5)
C-----
C-----            PRINT IIIII WITH NO MASTER FILE ENTRY
C
                  RETURN
              END IF
C
              IF (IRET.EQ.2 )
     C            THEN
C
C                     CORRUPT NUMBER   -   FATAL ERROR.
C
                      KPT=IABS(KPT)
                      KCHAR(KPT+5)=IOR(KCHAR(KPT+5),128)
                      KERR = 2
                      IF ( IFIRST.EQ.0 ) NOER(1,2)=NOER(1,2) + 1
C
C                     RETURN IF REPORT TOO SHORT TO CONTAIN IDENT
C
                      IF( (KPT+5) .GE. IEQ) RETURN
C
                      GO TO 4000
                  END IF
C
              IF (IRET.EQ.3) RETURN
C
C
C
C     FOR "NIL" REPORTS ONLY THE HEADER WILL BE DECODED
C
            IF(NIL .EQ. 1) THEN
               KDEC(20)=1
                GO TO 790
            END IF
C
C
C             IF WIND INDICATOR IW IS MISSING USE THE MASTER FILE
C             INDICATOR TO REPLACE IW.  REPLACEMENT IW INDICATES
C             WIND MEASURED AND UNITS KNOTS OR METRES PER SECOND.
C
              IF ( KINT(3).EQ.MINDIC )
     C            THEN
C
C                     KNOTS
C
                      IF ( IWIND.EQ.0 ) KINT(3)=4
C
C                     METRES PER SECOND
C
                      IF ( IWIND.EQ.1 ) KINT(3)=1
                  END IF
C
           ELSE
C
C              SEA STATION
C
C              RETAIN POINTER TO STATION IDENTIFIER .
C
               KPT=IPT
C
C              CHECK WHETHER BUOY REPORT OR SHIP,RIG OR PLATFORM.
C              BUOUY REPORTS HAVE D---D OF 5 FIGURES.
C
               IBUOY = 0
               K = IPT
               J = K + 4
C
               DO 750 I=K,J
                    IF ( KCHAR(I).GE.65.AND.KCHAR(I).LE.90 ) IBUOY=1
  750          CONTINUE
C
C              IF A 5 FIGURE GROUP IS FOUND CHECK FOLLOWING GROUP.
C              IF THE FOLLOWING GROUP STARTS WITH A 9 THE SHIP CALL
C              SIGN GROUP IS PRESUMED MISSING.
C
               CALL NEXTPRT (I,IEQ)
               IF ( IBUOY.EQ.0 )
     C             THEN
                       IF (KCHAR(I).EQ.57) IBUOY = 2
                   END IF
C
               IF ( IBUOY.NE.0 )
     C             THEN
C
C                      SHIP,RIG OR PLATFORM. IDENTIFIER NO. SET TO 0
C
                       KINT(4) = 0
C
                       IF (IBUOY.EQ.1)
     C                    THEN
C
C                             MOVE POINTER PAST CALL SIGN . THE FIRST
C                             FIGURE AFTER START OF THE NEXT GROUP IS
C                             LOCATED AS SOME SHIPS USE THE FULL NAME
C                             ( WHICH CAN CONSIST OF 2 WORDS ) INSTEAD
C                             OF CALL SIGN.
C
                              CALL NEXTVAL (IPT,32,IEQ)
                              CALL NEXTFIG (IPT,IEQ)
C
                              ID = IPT-KPT-1
C
                          ELSE
C
C                             D---D MISSING , SO SET LENGTH OF CALL
C                             SIGN TO 0.
C
                              ID = 0
C
                          END IF
C
                   ELSE
C
C                      BUOY- A1 BW NBNBNB
C
                       CALL EXTINT ( IPT,5,4 )
C
C                      ALTER RDB REPORT TYPE FIGURE
C
                       KDEC(4) = 24
                       ID = 5
C
                   END IF
C
C                  YY GG IW
C
                   CALL NEXTPRT ( IPT,IEQ )
C
C                  IF FIRST OF FIGURE OF GROUP IS NOT 0-3 , YYGGIW
C                  GROUP IS MISSING.
C
                   IF (KCHAR(IPT).GE.48.AND.KCHAR(IPT).LE.51)
     C                  CALL EXTGRP ( IPT,2,2,1,0,0,1,IRET )
                   JPT = IPT
C
C                  99 LALALA
C
                   CALL NEXTPRT ( IPT,IEQ )
                   CALL EXTGRP ( IPT,2,3,0,0,0,5,IRET )
C
C                  CHECK VALIDITY OF LALALA
C
                   IF (KINT(6).LT.0.OR.KINT(6).GT.900)
     C                         THEN
                                   IPT =IABS(IPT)
                                   KCHAR(IPT)=IOR(KCHAR(IPT),128)
                                   KERR = 4
                                   IF (IFIRST.EQ.0)
     C                                 NOER(1,4) = NOER(1,4) + 1
                                   GO TO 4000
                               END IF
C
C                  QC LOLOLOLO
C
                   CALL NEXTPRT ( IPT,IEQ )
                   CALL EXTGRP ( IPT,1,4,0,0,0,7,IRET )
C
C                  CHECK VALIDITY OF LOLOLOLO
C
                   IF (KINT(7).NE.1.AND.KINT(7).NE.3.AND.
     C                          KINT(7).NE.5.AND.KINT(7).NE.7) IRET=1
                   IF (KINT(8).LT.0.OR.KINT(8).GT.1800)IRET=1
                   IF (IRET.NE.0)
     C                        THEN
                                  IPT= IABS(IPT)
                                  KCHAR(IPT)=IOR(KCHAR(IPT),128)
                                  KERR=4
                                  IF ( IFIRST.EQ.0 )
     C                                NOER(1,4)=NOER(1,4) + 1
                                  GO TO 4000
                              END IF
C
           END IF
790   CONTINUE
C
C     RETAIN STATION IDENTIFIER IN CHARACTER FORM ( IF ANY EXISTS )
C
      IF ( ID.NE.0 )
     C    THEN
              IF (ID.GT.5) ID = 5
              J = KPT
              K = 9 + ID -1
              DO 800 I=9,K
C
C                  IF LETTER ENCOUNTERED FOR LAND STATION CONVERT
C                  IT TO FIGURE (NOT FOR SHIP'S CALL SIGN)
C
                   IF(KDEC(4) .LE. 14) CALL LETFIG(KCHAR(J))
                   KINT(I) = KCHAR(J)
                   J = J + 1
  800         CONTINUE
          END IF
C
C
      IF(NIL .EQ. 1) RETURN
C
C
C
C     CHECK VALUES OF YY GG IW FOR SHIP REPORTS. IF AN ERROR
C     IS FOUND IN YY OR GG THE ERROR IS FATAL AS SHIP REPORTS
C     FREQUENTLY HAVE DIFFERENT TIMES FROM BULLETIN HEADER TIMES.
C     IF THERE IS AN ERROR IN IW THE  WIND CANNOT BE DECODED .
C
      IF ( KDEC(4).GT.14 )
     C     THEN
               IRET = 0
               IF (KINT(1).LT.1.OR.KINT(1).GT.31)
     C             THEN
                       IRET = 2
                       KINT(1) = MINDIC
                   END IF
               IF (KINT(2).LT.0.OR.KINT(2).GT.23)
     C             THEN
                       IRET = 2
                       KINT(2) = MINDIC
                   END IF
               IF (KINT(3).EQ.2.OR.KINT(3).LT.0.OR.KINT(3).GT.4)
     C             THEN
                       IF (IRET.EQ.0) IRET = 1
                       KINT(3) = MINDIC
                   END IF
C
               IF (IRET.NE.0)
     C             THEN
                       JPT=IABS(JPT)
                       KCHAR(JPT)=IOR(KCHAR(JPT),128)
                       IF ( IFIRST.EQ.0 )
     C                      NOER(1,3) = NOER(1,3) + 1
                       IF ( IRET.EQ.2 )
     C                      THEN
                                KERR = 3
                                GO TO 4000
                            END IF
C
                   END IF
           END IF
C
C
C***
C*    CONVERT SECTION 1 .
C***
C
C     IR IX H VV
C
      CALL NEXTPRT ( IPT,IEQ )
      CALL EXTGRP ( IPT,1,1,1,2,0,14,IRET )
C
C     CHECK RANGES OF VALUES.
C
C     IR   0 - 4 OR /
C
C
      IF (KINT(14).LT.0.OR.KINT(14).GT.4.AND.
     C                KINT(14).NE.MINDIC) IRET = 1
C
C     IX  1 - 7 OR /
C
      IF (KINT(15).LT.1.OR.KINT(15).GT.7.AND.
     C                KINT(15).NE.MINDIC) IRET = 1
C
      IF (IRET.NE.0 )
     C    THEN
               IPT = IABS(IPT)
               KCHAR(IPT) = IOR(KCHAR(IPT),128)
               KERR = 5
               IF (IFIRST.EQ.0) NOER(1,5) = NOER(1,5) + 1
          END IF
C
C
C     IF AUTO REPORT ALTER RDB CODE FIGURE .
C
      IF ( KINT(15).GE.4.AND.KINT(15).LE.7)
     C                  THEN
                            IF (KDEC(4).EQ.11) KDEC(4) =14
                            IF (KDEC(4).EQ.21) KDEC(4) =24
                        END IF
C
C
C     N DD FF
C
      CALL NEXTPRT ( IPT,IEQ )
      CALL EXTGRP ( IPT,1,2,2,0,0,18,IRET )
C
C     CHECK RANGE OF VALUES.
C
      IF (KINT(19).LT.0) IRET = 1
      IF (KINT(19).GT.86.AND.KINT(19).NE.99) IRET = 1
      IF (KINT(19).GT.36.AND.KINT(19).LT.51) IRET = 1
      IF (KINT(19).EQ.0.AND.KINT(20).NE.0) IRET = 1
C
C     IF GROUP IS  ///// , N//FF OR N////  IT IS ACCEPTED.
C
      IF (KINT(19).EQ.MINDIC) IRET = 0
C
C     check if ff = 99
C
      if(kint(20).eq.99) then
c
C        LOCATE NEXT GROUP .
C
         CALL NEXTPRT ( IPT,IEQ )
         IF (IPT.GE.IEQ) GO TO 3000
         if(kchar(ipt).eq.48.and.kchar(ipt+1).eq.48) then
            CALL EXTGRP ( IPT,2,3,0,0,0,255,IRET )
         end if
      end if
C
      IF (IRET.NE.0) THEN
                         IPT = IABS(IPT)
                         KCHAR(IPT) = IOR(KCHAR(IPT),128)
                         KERR = 6
                         IF (IFIRST.EQ.0) NOER(1,6) = NOER(1,6)+1
                     END IF
C
C
C
C     LOCATE NEXT GROUP .
C
      CALL NEXTPRT ( IPT,IEQ )
      IF (IPT.GE.IEQ) GO TO 3000
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 1 ( '1' = 49 ) .
C
      IF ( KCHAR(IPT).EQ.49 )
     C     THEN
C
C              1 SN TTT
C
C              SHIP REPORTS IN REDUCED FORM USE TT/ , SO
C              REPLACE / BY '0' .
C
C              MODIFY RDB CODE FIGURE AS WELL.
C
               IF ( KCHAR(IPT+4).EQ.47 )
     C              THEN
                        KCHAR(IPT+4) = 48
                        IF(KDEC(4).GT.14) KDEC(4) = 23
                    END IF
C
               CALL EXTGRP( IPT,1,1,3,0,0,21,IRET )
C
               IF (KINT(22).LT.0.OR.KINT(22).GT.1) IRET = 1
C
C              IF SN  = / , GROUP IS TREATED AS 1////
C
               IF (KINT(22).EQ.MINDIC) IRET = 0
C
               IF ( IRET.NE.0 )
     C              THEN
                        IPT = IABS(IPT)
                        KCHAR(IPT)=IOR(KCHAR(IPT),128)
                        KERR = 7
                        IF (IFIRST.EQ.0)  NOER(1,7) =NOER(1,7) + 1
                     END IF
C
                CALL NEXTPRT ( IPT,IEQ )
           END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 2 ( '2' = 50 )
C     FIRST CHECK THAT IT IS NOT START OF SECTION 2.
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.50.AND.KCHAR(IPT+1).NE.50 )
     C     THEN
C
C              2 SN TDTDTD  OR   2 9 UUU
C
C              IF TDTDTD IS IN THE FORM TDTD/ , REPLACE / BY 0 .
C
               IF (KCHAR(IPT+4).EQ.47 ) KCHAR(IPT+4) = 48
C
               CALL EXTGRP ( IPT,1,1,3,0,0,24,IRET )
C
               IF (KINT(25).LT.0.OR.KINT(25).GT.9)IRET=1
               IF (KINT(25).GT.1.AND.KINT(25).LT.9) IRET = 1
C
C              IF SN = / , GROUP IS TREATED AS 2////
C
               IF (KINT(25).EQ.MINDIC) IRET = 0
C
               IF (IRET.NE.0)
     C              THEN
                        IPT = IABS(IPT)
                        KCHAR(IPT) = IOR(KCHAR(IPT),128)
                        IF (IFIRST.EQ.0)  NOER(1,8) = NOER(1,8)+1
                        KERR = 8
                     END IF
               CALL NEXTPRT ( IPT,IEQ )
           END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 3 ( '3' = 51 )
C     FIRST CHECK THAT IT IS NOT START OF SECTION 3.
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.51.AND.KCHAR(IPT+1).NE.51 )
     C     THEN
C
C              3 POPOPOPO
C
C              IF POPOPOPO IS OF THE FORM POPOPO/ , REPLACE / BY 0.
C
               IF (KCHAR(IPT+4).EQ.47) KCHAR(IPT+4) = 48
C
               CALL EXTGRP( IPT,1,4,0,0,0,27,IRET )
C
               IF (IRET.NE.0)
     C              THEN
                        IPT = IABS(IPT)
                        KCHAR(IPT)=IOR(KCHAR(IPT),128)
                        KERR = 9
                        IF (IFIRST.EQ.0) NOER(1,9) = NOER(1,9)+1
                    END IF
C
               CALL NEXTPRT ( IPT,IEQ )
           END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 4 ( '4' = 52 )
C     FIRST CHECK THAT IT IS NOT START OF SECTION 4.
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR (IPT).EQ.52.AND.KCHAR(IPT+1).NE.52 )
     C     THEN
C
C              4 P PPP    OR  4 A3 HHH
C
C              SHIP REPORTS IN REDUCED FORM USE PPP/ ,
C              SO REPLACE '/' BY '0' .
C
C              MODIFY RDB CODE FIGURE AS WELL.
C
               IF ( KCHAR(IPT+4).EQ.47 )
     C              THEN
                        KCHAR(IPT+4) = 48
                        IF(KDEC(4).GT.14) KDEC(4) = 23
                    END IF
C
               CALL EXTGRP( IPT,1,1,3,0,0,29,IRET )
C
               IF ( IRET.NE.0 )
     C              THEN
                        IPT = IABS(IPT)
                        KCHAR(IPT) = IOR(KCHAR(IPT),128)
                        IF (IFIRST.EQ.0) NOER(1,10)=NOER(1,10)+1
                        KERR = 10
                    END IF
C
               CALL NEXTPRT ( IPT,IEQ )
           END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 5 ( '5' = 53 )
C     FIRST CHECK THAT IT IS NOT START OF SECTION  5.
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.53.AND.KCHAR(IPT+3).NE.32)
     C                    THEN
C
C                             5 A PPP
C
                              CALL EXTGRP ( IPT,1,1,3,0,0,32,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 6 ( '6' = 54 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.54 )
     C                    THEN
C
C                             6 RRR TR
C
                              CALL EXTGRP ( IPT,1,3,1,0,0,35,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 7 ( '7' = 55 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.55 )
     C                    THEN
C
C                             7 WW W1 W2
C
                              CALL EXTGRP ( IPT,1,2,1,1,0,38,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 8 . ( '8' = 56 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.56 )
     C                    THEN
C
C                             8 NH CL CM CH
C
                              CALL EXTGRP ( IPT,1,1,1,1,1,42,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 9 . ( '9' = 57 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.57 )
     C                    THEN
C
C                             9 GGgg
C
                              CALL EXTGRP ( IPT,1,2,2,0,0,47,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C
C
C***
C*    CONVERT SECTION 2 .
C***
C
C     CONVERT 222 GROUP .( '2' = 50 )
C
  900 IF (IPT.GE.IEQ) GO TO 3000
      IF (KCHAR(IPT).NE.50 )
     C                   THEN
C
C                            NOT SECTION 2
C
                             GO TO 1000
                         ELSE
C
C                            222 DS VS
C
                             CALL EXTGRP ( IPT,3,1,1,0,0,50,IRET )
                             CALL NEXTPRT ( IPT,IEQ )
C
C                            ALTER RDB CODE FIGURE IF ABBREVIATED
C                              REPORT ( DSVS = // )
C
                             IF ( KDEC(4).EQ.21.AND.KINT(51).EQ.MINDIC.
     C                           AND.KINT(52).EQ.MINDIC ) KDEC(4) =22
                         END IF
C
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 0 ( '0' = 48 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.48 )
     C                    THEN
C
C                              0 SN TWTWTW
C
                               CALL EXTGRP (IPT,1,1,3,0,0,53,IRET )
                               CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C    CONVERT IF GROUP IDENTIFYING FIGURE IS 1 ( '1' = 49 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.49 )
     C                    THEN
C
C                              1 PWAPWA HWAHWA
C
                               CALL EXTGRP( IPT,1,2,2,0,0,56,IRET )
                               CALL NEXTPRT ( IPT,IEQ )
                           END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 2 ( '2' = 50 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.50 )
     C                    THEN
C
C                             2 PWPW HWHW
C
                              CALL EXTGRP( IPT,1,2,2,0,0,59,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 3 ( '3' = 51 )
C     FIRST CHECK IF START OF SECTION 3 .
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.51.AND.KCHAR(IPT+3).NE.32)
     C                    THEN
C
C                              3 DW1DW1 DW2DW2
C
                               CALL EXTGRP ( IPT,1,2,2,0,0,62,IRET)
                               CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 4 ( '4' = 52 )
C     FIRST CHECK IF START OF SECTION 4 .
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.52.AND.KCHAR(IPT+3).NE.32)
     C                    THEN
C
C                             4 PW1PW1 HW1HW1
C
                              CALL EXTGRP ( IPT,1,2,2,0,0,65,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 5 ( '5' = 53 )
C     FIRST CHECK IF START OF SECTION 5 .
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.53.AND.KCHAR(IPT+3).NE.32)
     C                    THEN
C
C                             5 PW2PW2 HW2HW2
C
                              CALL EXTGRP ( IPT,1,2,2,0,0,68,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 6 ( '6' = 54 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.54 )
     C                    THEN
C
C                             6 IS ESES RS
C
                              CALL EXTGRP ( IPT,1,1,2,1,0,71,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 70 ( '7' = 55 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.55.AND.KCHAR(IPT+1).EQ.48 ) THEN
C
C          70HwaHwaHwa
C
C
           CALL EXTGRP(IPT,2,3,0,0,0,250,IRET)
           CALL NEXTPRT ( IPT,IEQ )
      END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 8 ( '8' = 56 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.56) THEN
C
C          8swTbTbTb
C 
C
           CALL EXTGRP(IPT,1,1,3,0,0,252,IRET)
           CALL NEXTPRT ( IPT,IEQ )
      END IF
C
C     CHECK FOR 'ICE' INDICATOR.
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.73 )
     C                    THEN
C
C                             ICE , SO SET FLAG.
C
                              KINT(75) = 1
                              IPT =IPT + 3
                              CALL NEXTPRT( IPT,IEQ )
C
C                             CI SI BI DI ZI
C
                              CALL EXTGRP ( IPT,1,1,1,1,1,76,IRET )
                              CALL NEXTPRT (IPT,IEQ)
                          END IF
C
C*    CONVERT SECTION 3.
C***
C
 1000 IF (IPT.GE.IEQ) GO TO 3000
      IF (KCHAR(IPT).NE.51)
     C                    THEN
C
C                             NOT SECTION 3
C
                              GO TO 2000
                          ELSE
C
C                             333 GROUP
C
                              CALL EXTGRP ( IPT,3,0,0,0,0,81,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 0 ( '0' = 48 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.48 )
     C                    THEN
C
                              IF ( KDEC(17).EQ.1 )
     C                               THEN
C
C                                        REGION 1
C
C                                        O  TGTG  RC  RT
C
                                         CALL EXTGRP
     C                                      (IPT,1,2,1,1,0,82,IRET)
                                         CALL NEXTPRT(IPT,IEQ)
                                     ELSE
C
C                                        REGION 4
C
C                                        0  CS  DL  DM  DH
C
                                         CALL EXTGRP
     C                                       (IPT,1,1,1,1,1,82,IRET)
                                         CALL NEXTPRT(IPT,IEQ)
                                     END IF
                        END IF
C
C
C
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 1 ( '1' = 49 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.49 )
     C                    THEN
C
C                             1 SN TXTXTX
C
                              CALL EXTGRP ( IPT,1,1,3,0,0,87,IRET )
                              CALL NEXTPRT( IPT,IEQ )
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 2 ( '2' = 50 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.50 )
     C                    THEN
C
C                             2 SN TNTNTN
C
                              CALL EXTGRP ( IPT,1,1,3,0,0,90,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 3 ( '3' = 51 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.51 )
     C                    THEN
C
C                             3 E SN TGTG
C
                              CALL EXTGRP ( IPT,1,1,1,2,0,93,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 4 ( '4' = 52 )
C     FIRST CHECK IF START OF SECTION 4 .
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.52.AND.KCHAR(IPT+3).NE.32)
     C                    THEN
C
C                             4  E'  SSS
C
                              CALL EXTGRP (IPT,1,1,3,0,0,98,IRET)
                              CALL NEXTPRT (IPT,IEQ)
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 5 ( '5' = 53 )
C     FIRST CHECK IF START OF SECTION 5 .
C
 1050 IF (IPT.GE.IEQ) GO TO 3000
      IF (KCHAR(IPT).EQ.53.AND.KCHAR(IPT+3).NE.32) THEN
C
C        5  ?
C
         CALL EXTINT (IPT,1,101)
         CALL EXTINT (IPT,1,102)
         IF(IPT.LT.0) THEN
            IPT=IABS(IPT)
C
C           SKIP PROBLEM GROUP
C
            CALL NEXTPRT(IPT,IEQ)
            CALL NEXTSEP(IPT,IEQ)
            GO TO 1050
         END IF
         CALL EXTINT (IPT,1,103)
         IF(IPT.LT.0) THEN
            IPT=IABS(IPT)
C
C           SKIP PROBLEM GROUP
C
            CALL NEXTPRT(IPT,IEQ)
            CALL NEXTSEP(IPT,IEQ)
            GO TO 1050
         END IF
         IPT = IABS(IPT)
         IPT = IPT-3
         IC1=KINT(101)
         IC2=KINT(102)
         IC3=KINT(103)
C
C        IF ? IS 8 OR9  P24P24P24
C
         IF (IC2.EQ.8) THEN
             CALL EXTGRP (IPT,1,1,3,0,0,233,IRET)
             CALL NEXTPRT (IPT,IEQ)
         END IF
         IF (IC2.EQ.9) THEN
             CALL EXTGRP (IPT,1,1,3,0,0,236,IRET)
             CALL NEXTPRT (IPT,IEQ)
         END IF
C
C        IF ? IS 4 , 6 OR 7 THEN PARAMS ARE
C        GO SN DT , DL DM DH OR C DA EC . USE
C        DEPENDS ON REGION.
C
C              54 g0 Sn dT (TEMPERATURE CHANGE DATA IN PERIOD COV. BY W1W2
C
         IF (IC2.EQ.4) THEN
            CALL EXTGRP(IPT,1,1,1,1,1,184,IRET)
            CALL NEXTPRT (IPT,IEQ)
         END IF
C
C              56 DL DM DH   DIRECTION ON CLOUD DRIFT
C
         IF (IC2.EQ.6) THEN
            CALL EXTGRP(IPT,1,1,1,1,1,223,IRET)
            CALL NEXTPRT (IPT,IEQ)
         END IF
C
C              57 C Da eC   DIRECTION AND ELEVATION OF CLOUD
C
         IF (IC2.EQ.7) THEN
            CALL EXTGRP(IPT,1,1,1,1,1,228,IRET)
            CALL NEXTPRT (IPT,IEQ)
         END IF
C
C        IF ? IS 5  SSS  (SUNSHINE)
C
         IF (IC2.EQ.5.AND.IC3.NE.3) THEN
            CALL EXTGRP(IPT,1,1,3,0,0,189,IRET)
            CALL NEXTPRT (IPT,IEQ)
C
C               RADIATION DATA
C
            IF(KCHAR(IPT).EQ.48) THEN
C
C              POSITIVE NET RADIATION DURING THE PRECEDING 24 H
C
               CALL EXTGRP(IPT,1,4,0,0,0,192,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF(KCHAR(IPT).EQ.49) THEN
C
C              NEGATIVE NET RADIATION DURING THE PRECEDING 24 H
C
               CALL EXTGRP(IPT,1,4,0,0,0,194,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF(KCHAR(IPT).EQ.50) THEN
C
C              GLOBAL SOLAR RADIATION DURING THE PRECEDING 24 H
C
               CALL EXTGRP(IPT,1,4,0,0,0,196,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF(KCHAR(IPT).EQ.51) THEN
C
C              DIFFUSED SOLAR RADIATION DURING PRECEDING 24 H
C
               CALL EXTGRP(IPT,1,4,0,0,0,198,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF(KCHAR(IPT).EQ.52) THEN
C
C              DOWNWARD LONG WAVE RADIATION DURING PRECEDING 24 H
C
               CALL EXTGRP(IPT,1,4,0,0,0,200,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF(KCHAR(IPT).EQ.53) THEN
C
C              UPWARD LONG WAVE RADIATION DURING PRECEDING 24 H
C
               CALL EXTGRP(IPT,1,4,0,0,0,202,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF(KCHAR(IPT).EQ.54) THEN
C
C              SHORT WAVE RADIATION DURING PRECEDING 24 H
C
C              It is not unambiguous if radiation or precipitation
C              group follow. Check if there are 2 6???? groups
               ippt=ipt
               call nextsep(ippt,ieq)
               call nextprt(ippt,ieq)
               if(kchar(ippt).eq.54) then
               CALL EXTGRP(IPT,1,4,0,0,0,204,IRET)
               CALL NEXTPRT (IPT,IEQ)
               end if
            END IF
         END IF
         IF (IC2.EQ.5.AND.IC3.EQ.3) THEN
            CALL EXTGRP(IPT,1,2,2,0,0,206,IRET)
            CALL NEXTPRT (IPT,IEQ)
C
C               RADIATION DATA
C
            IF(KCHAR(IPT).EQ.48) THEN
C
C              POSITIVE NET RADIATION DURING THE PREVIOUS HOUR
C
               CALL EXTGRP(IPT,1,4,0,0,0,209,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF(KCHAR(IPT).EQ.49) THEN
C
C              NEGATIVE NET RADIATION DURING THE PREVIOUS HOUR
C
               CALL EXTGRP(IPT,1,4,0,0,0,211,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF(KCHAR(IPT).EQ.50) THEN
C
C              GLOBAL SOLAR RADIATION DURING THE PREVIOUS HOUR
C
               CALL EXTGRP(IPT,1,4,0,0,0,213,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF(KCHAR(IPT).EQ.51) THEN
C
C              DIFFUSED SOLAR RADIATION DURING THE PREVIOUS HOUR 
C
               CALL EXTGRP(IPT,1,4,0,0,0,215,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF(KCHAR(IPT).EQ.52) THEN
C
C              DOWNWARD LONG WAVE RADIATION DURING THE PREVIOUS HOUR
C
               CALL EXTGRP(IPT,1,4,0,0,0,217,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF(KCHAR(IPT).EQ.53) THEN
C
C              UPWARD LONG WAVE RADIATION DURING THE PREVIOUS HOUR
C
               CALL EXTGRP(IPT,1,4,0,0,0,219,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF(KCHAR(IPT).EQ.54) THEN
C
C              SHORT WAVE RADIATION DURING THE PREVIOUS HOUR
C
               CALL EXTGRP(IPT,1,4,0,0,0,221,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
         END IF
C
C        IF ? IS 0,1,2,3 OR EEEIE (EVAPOTRANSPIRATION)
C
         IF(IC2.GE.0.AND.IC2.LE.3.OR.IC2.EQ.MINDIC) THEN
            CALL EXTGRP(IPT,1,3,1,0,0,181,IRET)
            CALL NEXTPRT (IPT,IEQ)
         END IF
C
C        SKIP GROUP IF SECOND CHARACTER INVALID.
C
         IF(IC2.LT.0.OR.IC2.GT.9.AND.IC2.NE.MINDIC) THEN
            IPT = IPT + 5
            CALL NEXTPRT (IPT,IEQ)
         END IF
C
C        INTERMEDIATE FORMAT ACCOMMODATES ONLY 1
C        5-GROUP . OVERWRITE IF A SECOND GROUP.
C
         IF ( KCHAR(IPT).EQ.53 ) GO TO 1050
C
      END IF
C
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 6 ( '6' = 54 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.54 )
     C                    THEN
C
C                             6  RRR  TR
C
                              CALL EXTGRP ( IPT,1,3,1,0,0,113,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 7 ( '7' = 55 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.55 )
     C                    THEN
C
C                             7  R24R24R24R24
C
                              CALL EXTGRP ( IPT,1,4,0,0,0,116,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 8 ( '8' = 56 )
C     CAN ACCEPT UP TO 4 SUCH GROUPS.
C
      N = 121
      DO 1100 I=1,4
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.56 )
     C                    THEN
C
C                             8 NS C HSHS
C
                              CALL EXTGRP (IPT,1,1,1,2,0,N,IRET)
                              N = N + 4
                              CALL NEXTPRT (IPT,IEQ)
                          END IF
 1100 CONTINUE
C
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 9 ( '9' = 57 )
C     CAN BE UP TO 4 SUCH GROUPS.
C
      N = 137
      DO 1200 I=1,4
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.57 )
     C                    THEN
C
C                             9  SPSP  SPSP
C
                              CALL EXTGRP ( IPT,1,2,2,0,0,N,IRET )
                              N = N + 3
                              CALL NEXTPRT ( IPT,IEQ )
                         END IF
 1200 CONTINUE
C
C
C
C
C***
C*    CONVERT SECTION 4.
C***
C
 2000 IF (IPT.GE.IEQ) GO TO 3000
      IF (KCHAR(IPT).NE.52)
     C                   THEN
C
C                            NOT SECTION 4
C
                             GO TO 3000
                         ELSE
C
C                            444 N' C' H'H' Ct
C
                             CALL EXTINT (IPT,3,149)
                             CALL NEXTPRT ( IPT,IEQ )
                             CALL EXTGRP (IPT,1,1,2,1,0,150,IRET)
                             CALL NEXTPRT ( IPT,IEQ )
                         END IF
C
C
C***
C*    SECTION 5 . NATIONAL GROUPS NOT USED.
C***
C
C     ERROR IF NOT SECTION 5 AND NOT END OF REPORT.
C
 3000 IF (KCHAR(IPT).NE.53.AND.IPT.LT.IEQ)
     C    THEN
              KERR = 0
              KCHAR(IEQ) = IOR(KCHAR(IEQ),128)
              IF (IFIRST.EQ.0) NOER(1,60) = NOER(1,60) + 1
          END IF
C
C
C***
C*    ERROR CHECKING AND HANDLING.
C***
C
C     RETURN IF NO ERROR IN REPORT.
C
 4000 IF ( KERR.EQ.0 ) RETURN
C
C     IF FIRST DECODING ATTEMPT , TRY TO CORRECT ERROR AND DECODE
C     AGAIN.
C
      IF ( IFIRST.EQ.0 ) THEN
                        IPT = KEEP
                        CALL FIXSM
                        IFIRST = 1
                        GO TO 10
                    ELSE
                        CALL SAVREP( IHEAD,IERR )
C
C
C                       CLEAR PARITY BIT AFTER SAVING ERROR FILE
C
                        DO 4100 I=KEEP,IGS
                        KCHAR(I) = IAND(KCHAR(I) , 127)
4100                    CONTINUE
C
C                       ONLY REPORTS WITH ERROR IN DATE/TIME OR
C                       LAT/LONG ARE NOT PROCESSED FURTHER.
C
                        IF (KERR.GT.4) KERR = 0
                        RETURN
                    END IF
C
      END
      SUBROUTINE FIXSM
C
C
C**** *FIXSM*
C
C
C     PURPOSE.
C     --------
C
C
C         *CALL* *FIXSM*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
      INCLUDE 'comindx.h'
C
C     ------------------------------------------------------------------
C*          1.  CLEAR PARITY BITS.
C                -----------------
 100  CONTINUE
C
      DO 101 I=IPT,IGS
         KCHAR(I) = IAND(KCHAR(I),127)
  101 CONTINUE
C
      ICOR = 0
C
      CALL REMEEE
C
C*    TEST IF SEA STATION REPORT.
C
      IF ( KDEC(4).GT.14 ) GO TO 5000
C
C*    CLEAR SPURIOUS SM CHARACTERS FROM KWBC REPORTS
C
C
      K = IPT + 10
      DO 200 I=IPT,K
         IF ( KCHAR(I).EQ.32.AND.KCHAR(I+1).EQ.83.AND.KCHAR(I+2).
     C      EQ.77.AND.KCHAR(I+3).EQ.32)
     C           THEN
C---                 WRITE (*,9901) ICOR
C---                 CALL PRTBULL (IPT,IEQ)
                     KCHAR(I+1) = 32
                     KCHAR(I+2) = 32
                     ICOR = 1
                 END IF
C
  200 CONTINUE
C
C
C*    SOME CENTRES USE '333' GROUP IN THE FORM '333//'
C
C
      DO 250 I=IPT,IEQ
         IF ( KCHAR(I).EQ.51.AND.KCHAR(I+1).EQ.51.AND.KCHAR(I+2).
     C      EQ.51.AND.KCHAR(I+3).EQ.47.AND.KCHAR(I+4).EQ.47)
     C           THEN
C---                 WRITE (*,9901) ICOR
C---                 CALL PRTBULL (IPT,IEQ)
                     KCHAR(I+3) = 32
                     KCHAR(I+4) = 32
                     ICOR = 1
                 END IF
  250 CONTINUE
C
C
C*    FIXUP COMMON ERRORS IN IIIII GROUP FROM LAND STATIONS.
C
C
C     REMOVE EXTRA CHARACTER PRECEDING IIIII IN REPORTS FROM
C     MXKF,AMMC,NZKL AND EESA.
C
      K = IPT+5
      DO 300 I=IPT,K
         IF ( KCHAR(I).GE.48.AND.KCHAR(I).LE.57) GO TO 350
         IF ( KCHAR(I).GE.65.AND.KCHAR(I).LE.90) GO TO 350
         IF (KCHAR(I).EQ.32) GO TO 300
C--      WRITE (*,9901) ICOR
C--      CALL PRTBULL (IPT,IEQ)
         ICOR = 2
         KCHAR(I) = 32
  300 CONTINUE
  350 IF (ICOR.EQ.2) CALL NEXTPRT (IPT,IEQ)
C
C     ADD II OF 03 TO U.K. REPORTS FROM CENTRES OTHER THAN EGRR.
C
      K = IAH + 4
      CALL NEXTLET (K,JAH)
      IF (KCHAR(K).EQ.69.AND.KCHAR(K+1).EQ.71)
     C          THEN
                    IF (KCHAR(K+2).EQ.82.AND.KCHAR(K+3).EQ.82)
     C                THEN
C----                     WRITE (*,9901) ICOR
C----                     CALL PRTBULL (IPT,IEQ)
                          IPT = IPT - 2
                          KCHAR(IPT) = 48
                          KCHAR(IPT+1) = 51
                          ICOR = 3
                      END IF
                END IF
C
C
C     REMOVE ANY EXTRA SHORT GROUPS BEFORE IIIII
C
      K = IPT
      CALL NEXTVAL (K,32,IEQ)
      K = K - IPT
      IF (K.LE.3) THEN
C--                   WRITE (*,9901) ICOR
C--                   CALL PRTBULL (IPT,IEQ)
                      IPT = K+ IPT
                      CALL NEXTPRT (IPT,IEQ)
                      ICOR = 4
                  END IF
C
C
C***
C*    COMMON FORMAT ERRORS IN REPORTS FROM SOUTH AMERICA
C     AND AFRICA
C
C
      DO 400 K=IPT,IEQ
C
C          $,],:,*,V OR ? INSTEAD OF = AT END OF REPORT
C         IF ANY OF THESE CHARACTERS IS FOLLOWED BY LF IT IS
C         REPLACED BY = .
C
           IF (KCHAR(K).EQ.36.OR.KCHAR(K).EQ.93.OR.KCHAR(K)
     C        .EQ.58.OR.KCHAR(K).EQ.42.OR.KCHAR(K).EQ.86
     C        .OR.KCHAR(K).EQ.63.AND.KCHAR(K+1).EQ.13)
     C           THEN
C--                  WRITE (*,9901) ICOR
C--                  CALL PRTBULL (IPT,IEQ)
                     KCHAR(K) = 61
                     ICOR = 5
                 END IF
C
C          = SIGN MISSING AT END OF NIL REPORTS
C
           IF (KCHAR(K).EQ.76.AND.KCHAR(K+1).EQ.13)
     C           THEN
C--                  WRITE (*,9901) ICOR
C--                  CALL PRTBULL (IPT,IEQ)
                     KCHAR(K) = 61
                     ICOR = 6
                 END IF
C
C          - INSTEAD OF SPACE
C
           IF (KCHAR(K).EQ.45)
     C           THEN
C--                  WRITE (*,9901) ICOR
C--                  CALL PRTBULL (IPT,IEQ)
                     KCHAR(K) = 32
                     ICOR = 7
                 END IF
C
C          $ INSTEAD OF =
C
           IF (KCHAR(K).EQ.36)
     C           THEN
C--                  WRITE (*,9901) ICOR
C--                  CALL PRTBULL (IPT,IEQ)
                     KCHAR(K) = 61
                     ICOR = 8
                 END IF
C
C
  400 CONTINUE
C
C     RESET POINTER TO END OF REPORT
C
      IEQ = IPT
      CALL NEXTEQ (IEQ,IGS)
C
C
C     IF NO END OF REPORT HAS BEEN FOUND INSERT = AT END OF
C     LINE . THIS ENSURES THAT AT LEAST SECTION 1 OF REPORTS IS
C     DECODED.
C
C     IF THE REPORT IS LONGER THAN 144 CHARACTERS = IS ALSO
C     PRESUMED MISSING.
C
      LEN = IEQ - IABS(IPT)
C
      IF (IEQ.GE.IGS.OR.LEN.GE.144)
     C      THEN
C--           WRITE (*,9901) ICOR
C--           CALL PRTBULL (IPT,IEQ)
              K = IPT
              CALL NEXTEND (K,IGS)
              IEQ = K
              KCHAR(K) = 61
              ICOR = 9
C
            END IF
C
C
C
C
 5000 CONTINUE
      IF (ICOR.EQ.0) RETURN
      IF (ICOR.EQ.3) RETURN
C--   CALL PRTBULL (IPT,IEQ)
C--   WRITE (*,9901)ICOR
C
 9901 FORMAT (1H ,'***************',I3,' *********************')
C
C
C
      RETURN
      END
      SUBROUTINE ICHWHW(IN,MINDIC,OUT)
C
C
C****
C*
C*    NAME     : ICHWHW
C*
C*    FUNCTION :  DECODE THE HEIGHT OF WAVES IN DECIMETERS.
C*
C*    INPUT    :  IN      - CODE FIGURE FOR THE HEIGHT
C*             :  MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUT     - DECODED HEIGHT
C*
C*             OUT IS SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN IN
C*
C****
C
      INTEGER OUT
C
C***   SET MISSING VALUE
C
      OUT=MINDIC
C
      IF(IN .EQ. MINDIC) RETURN
C
      OUT=IN*5
C
      RETURN
      END
      SUBROUTINE IC2700(ICODE,ICOVER)
C
C
C**** *IC2700*
C
C
C     PURPOSE.
C     --------
C         TO CONVERT CODE TABLE 2700 INTO PERCENTAGE CLOUD COVERAGE.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *IC2700(ICODE,ICOVER)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      DIMENSION ICT(10),IPR(10)
      DATA ICT/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9/
      DATA IPR/ 0,10,25,40,50,60,75,90,100,113/
C
C     ------------------------------------------------------------------
C*          1.   CONVERT CLOUD COVERAGE IN OKTAS INTO PERCENTAGE.
C                ------------------------------------------------
 100  CONTINUE
C
      DO 101 I=1,10
C
      IF(ICODE.EQ.ICT(I)) THEN
                             ICOVER=IPR(I)
                             GO TO 200
                          END IF
 101  CONTINUE
C
      ICOVER=999999
C
 200  CONTINUE
C
      RETURN
      END 
      SUBROUTINE IC1751(ICODE,ICOVER)
C
C
C**** *IC1751*
C
C
C     PURPOSE.
C     --------
C         TO CONVERT CODE TABLE 1751 INTO BUFR TABLE 20033.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *IC1751(ICODE,ICOVER)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      DIMENSION ICT(5),IPR(5)
      DATA ICT/  1, 2, 3, 4, 5/
      DATA IPR/  1, 2, 3, 4, 5/
C
C     ------------------------------------------------------------------
C*          1.   CONVERT CLOUD COVERAGE IN OKTAS INTO PERCENTAGE.
C                ------------------------------------------------
 100  CONTINUE
C
      DO 101 I=1,5 
C
      IF(ICODE.EQ.ICT(I)) THEN
                             ICOVER=IPR(I)
                             GO TO 200
                          END IF
 101  CONTINUE
C
      ICOVER=999999
C
 200  CONTINUE
C
      RETURN
      END 
      SUBROUTINE IC3551(ICODE,ICOVER)
C
C
C**** *IC3551*
C
C
C     PURPOSE.
C     --------
C         TO CONVERT CODE TABLE 3551 INTO BUFR TABLE 20032.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *IC3551(ICODE,ICOVER)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      DIMENSION ICT( 5),IPR( 5)
      DATA ICT/ 0, 1, 2, 3, 4/
      DATA IPR/ 0, 1, 2, 3, 4/
C
C     ------------------------------------------------------------------
C*          1.   CONVERT CLOUD COVERAGE IN OKTAS INTO PERCENTAGE.
C                ------------------------------------------------
 100  CONTINUE
C
      DO 101 I=1,5 
C
      IF(ICODE.EQ.ICT(I)) THEN
                             ICOVER=IPR(I)
                             GO TO 200
                          END IF
 101  CONTINUE
C
      ICOVER=999999
C
 200  CONTINUE
C
      RETURN
      END 
      SUBROUTINE IC639(ICODE,ICOVER)
C
C
C**** *IC639*
C
C
C     PURPOSE.
C     --------
C         TO CONVERT CODE TABLE 639 INTO BUFR TABLE 20034.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *IC639(ICODE,ICOVER)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      DIMENSION ICT(10),IPR(10)
      DATA ICT/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9/
      DATA IPR/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9/
C
C     ------------------------------------------------------------------
C*          1.   CONVERT CLOUD COVERAGE IN OKTAS INTO PERCENTAGE.
C                ------------------------------------------------
 100  CONTINUE
C
      DO 101 I=1,10
C
      IF(ICODE.EQ.ICT(I)) THEN
                             ICOVER=IPR(I)
                             GO TO 200
                          END IF
 101  CONTINUE
C
      ICOVER=999999
C
 200  CONTINUE
C
      RETURN
      END 
      SUBROUTINE ICPWPW(IN,MINDIC,OUT)
C
C
C****
C*
C*    NAME     : ICPWPW
C*
C*    FUNCTION :  DECODE THE PERIOD OF WAVES IN SEC
C*
C*    INPUT    :  IN      - CODE FIGURE FOR THE PERIOD
C*             :  MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUT     - THE DECODED PERIOD
C*
C*             OUT IS SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN IN
C*
C****
C
      INTEGER OUT
C
C***   SET MISSING VALUE
C
      OUT=MINDIC
C
      IF(IN .EQ. MINDIC) RETURN
C
      OUT= IN
      IF(IN .EQ. 99) OUT=126
C
      RETURN
      END
      SUBROUTINE ICTRTR (ICOUNT,IREG,KTR,IHOURS,MINDIC,ITR)
C
C****
C*
C*    NAME     : ICTRTR
C*
C*    FUNCTION :  DETERMINE DURATION OF RAINFALL . REGIONAL AND
C*                NATIONAL PRACTICES ARE HANDLED.
C*
C*    INPUT    :  IREG   : WMO REGION NUMBER
C*                ICOUNT : WMO COUNTRY NUMBER
C*                KTR    : CODE FIGURE FOR 'TR'
C*                IHOURS : REPORT TIME (HOURS)
C*                MINDIC : MISSING DATA VALUE
C*
C*    OUTPUT   :  ITR    :  MEASUREMENT PERIOD FOR RAINFALL (HOURS)
C*
C*                       ITR IS SET TO MISSING VALUE
C*                       IF ANY ERRORS IN IREG OR IHOURS
C*
C***
C
      DIMENSION IDURAT(28)
C
      DATA IDURAT / 6, 24,  6, 12, 12, -9, 12, -9,
     *              6,  6, 24,  6,  6,  6,  6,  6,
     C             24,  6,  6,  6,  6, 12,  6, 12,
     C             24,  6, 12, 18 /
C
C
C
C***   SET MISSING VALUE
C
      ITR=MINDIC
C
C
      IF (IREG.LT.1.OR.IREG.GT.7) RETURN
      IF(IHOURS .LT. 0 .OR. IHOURS .GT. 24) RETURN
C
C***
C*    VALID 'TR' CODE FIGURE REPORTED.
C***
C
c     IF (KTR.NE.MINDIC) THEN
c                            ITR = KTR * 6
c                            RETURN
c                        END IF
C
      if (ktr.ne.mindic) then
          if(ktr.ge.1.and.ktr.le.4) then
             ITR = KTR * 6
             RETURN
          elseif(ktr.eq.5) then
             itr=1
          elseif(ktr.eq.6) then
             itr=2
          elseif(ktr.eq.7) then
             itr=3
          elseif(ktr.eq.8) then
             itr=9
          elseif(ktr.eq.9) then
             itr=15
          elseif(ktr.eq.0) then
             itr=mindic
          else
             itr=mindic
          end if
          return
      end if
C
C***
C*    NO 'TR' FIGURE . GROUP MAY HAVE BEEN OMITTED BECAUSE RRR = 0
C*    OR BECAUSE NATIONAL PRACTICE IS TO CODE A / .
C***
C
C     ARRAY IDURAT IS USED TO DEFINE THE PERIOD FOR
C     DIFFERENT REGIONS AND DIFFERENT REPORT TIMES
C
C
C      REGION I     00 & 12 Z        6
C                      18 Z         12
C                      06 Z         24
C      REGION II    00 & 12 Z       12
C                   06 & 18 Z     MINDIC
C      REGION III   00&06&12 Z       6
C                      12 Z         24
C      REGION IV       ALL           6
C      REGION V        00 Z         24
C                   06&12&18 Z       6
C      REGION VI    00 & 12 Z        6
C                   06 & 18 Z       12
C
C
C     SOME OBSERVATIONS ARE MADE + OR - 1 HOUR FROM STANDARD
C     MAIN HOURS.
C
      K = IHOURS
      IF ( K.EQ.1.OR.K.EQ.23 ) K = 0
      IF ( K.EQ.7.OR.K.EQ.5  ) K = 6
      IF ( K.EQ.13.OR.K.EQ.11) K = 12
      IF ( K.EQ.19.OR.K.EQ.17) K = 18
C
C     IND IS THE INDEX TO DEFINE THE REGION AND REPORT TIME
C
      IND=(IREG-1)*4 + K/6 +1
C
C
C     SOME COUNTRIES IN REGION 2 HAVE NATIONAL PRACTICES.
C
      IF ( IREG.NE.2 ) THEN
                           ITR = IDURAT(IND)
                           RETURN
                       END IF
C
C***
C*    REGION 2 NATIONAL PRACTICES.
C***
C
C     SAUDI ARABIA . 'TR' ALWAYS REPORTED AS / . PERIOD IS 12 HOURS.
C     COUNTRY NUMBER IS 020 ( REGION 2 ).
C
      IF ( ICOUNT.EQ.20 ) THEN
                                 ITR = 12
                                 RETURN
                             END IF
C
C     CHINA . PERIOD IS ALWAYS 6 HOURS.
C     COUNTRY NUMBER IS 250 ( REGION 2 ).
C
      IF ( ICOUNT.EQ.250 ) THEN
                                 ITR = 6
                                 RETURN
                             END IF
C
C     INDIA AND SRI LANKA . PERIOD IS FROM 0300.
C     COUNTRY NUMBERS FOR INDIA ARE 100 AND 110 AND FOR SRI LANKA 120.
C
      IF ( ICOUNT.GE.100.AND.ICOUNT.LE.120 )
     C     THEN
               ITR = IHOURS - 3
               IF (ITR.LE.0) ITR = ITR + 24
               RETURN
           END IF
C
C***
C*    REGION 2 REGIONAL PRACTICE.
C***
C
      IF(IDURAT(IND) .EQ. -9) RETURN
      ITR=IDURAT(IND)
C
      RETURN
      END
      SUBROUTINE IC0264(INA3,MINDIC,OUTA3)
C
C****
C*
C*    NAME     : IC0264
C*
C*    FUNCTION :  DECODE THE INDICATOR OF STANDARD ISOBARIC
C*                SURFACE A3 IN HECTOPASCAL
C*
C*    INPUT    :  INA3    - CODE FIGURE FOR A3
C*                MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTA3   - DECODED A3
C*
C*             OUTA3 IS SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN INA3
C*
C****
C
      INTEGER OUTA3
C
C***   SET MISSING VALUE
C
      OUTA3=MINDIC
C
      IF(INA3 .EQ. MINDIC) RETURN
      IF(INA3 .LE. 0 .OR. INA3 .GE. 9) RETURN
C
      GO TO (100,900,1000,1000,500,900,700,850) INA3
C
100   OUTA3=1000
      RETURN
C
500   OUTA3=500
      RETURN
C
700   OUTA3=700
      RETURN
C
850   OUTA3=850
      RETURN
C
900   OUTA3=925
      RETURN
C
1000  RETURN
C
      END
      SUBROUTINE IC0700(INDD,MINDIC,OUTDD)
C
C
C****
C*
C*    NAME     : IC0700
C*
C*    FUNCTION :  DECODE THE DIRECTION FROM WHICH SURF. WIND IS
C*                BLOWING, OR THE DIRECTION OF THE SHIP (D,DS,...)
C*
C*    INPUT    :  INDD    - DIRECTION CODE FIGURE
C*                MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTDD  - DECODED DIRECTION IN DEGREES
C*
C*             OUTDD IS SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN INDD
C*
C****
C
      INTEGER OUTDD
C
C
      DIMENSION IDIR(9)
C
      DATA IDIR/0,45,90,135,180,225,270,315,360/
C
C***   SET MISSING VALUE
C
      OUTDD=MINDIC
C
      IF(INDD .LT. 0 .OR. INDD .GT. 8) RETURN
C
      OUTDD=IDIR(INDD+1)
C
      RETURN
      END
      SUBROUTINE IC0777(IDD,ITEMP,MINDIC,IDEWPT)
C
C
C****
C*
C*    NAME     :  IC0777
C*
C*    FUNCTION :  DECODE DEW-POINT TEMPERATURE IN TENS OF DEGREE
C*
C*    INPUT    :  IDD     - DEW-POINT EPRESSION
C*             :  ITEMP   - TEMPERATURE
C*             :  MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  IDEWPT  - DECODED DEW-POINT TEMPERATURE
C*
C*             IDEWPT IS SET TO MISSING VALUE IF
C*             ANY ERRORS FOUND IN IDD
C*
C*   A.HOLOPAINEN  JAN.83
C*
C****
C
C
C
C***   CHECK MISSING DATA INDICATOR
C
      IF(IDD .EQ. MINDIC) RETURN
C
      IF(ITEMP .EQ. MINDIC) RETURN
C
C     CHECK THE RANGE OF IDD
C
      IF(IDD .LT. 0 .OR. IDD .GT. 99) RETURN
C
      IF(IDD .GT. 51 .AND. IDD .LE. 55) RETURN
C
C
      IDEW=IDD
      IF(IDD .GE. 56) IDEW=10 * (IDD - 50)
C
      IDEWPT=ITEMP - IDEW
C
      RETURN
      END
      SUBROUTINE IC0877(IDD,IFF,IWW,ICOUNT,MINDIC,IDIR,ISPEED)
C
C
C****
C*
C*    NAME     :  IC0877
C*
C*    FUNCTION :  DECODE WIND DIRECTION AND SPEED
C*
C*    INPUT    :  IDD     - WIND DIRECTION IN TENS OF DEGREE
C*             :  IFF     - WIND SPEED
C*             :  IWW     - WIND SPEED INDICATOR , CODE TABLE 1855
C*             :  ICOUNT  - COUNTRY NUMBER
C*             :  MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  IDIR    - DECODED WIND DIRECTION
C*                ISPEED  - DECODED WIND SPEED
C*
C*             IDIR AND ISPEED ARE SET TO MISSING VALUE IF
C*             ANY ERRORS FOUND IN IDD, IFF OR IWW
C*
C****
C
C***   SET MISSING VALUE
C
      IDIR=MINDIC
      ISPEED=MINDIC
C
C
C
C***   CHECK MISSING DATA INDICATOR
C
      IF(IWW .EQ. MINDIC .OR. IDD .EQ. MINDIC .OR.
     1   IFF .EQ. MINDIC) RETURN
C
C     CHECK IF WIND INDICATOR IR IS CORRECT
C
      IF(IWW .NE. 0 .AND. IWW .NE. 1 .AND. IWW .NE. 3
     1  .AND. IWW .NE. 4) RETURN
C
C
C     IW IS THE MODIFIED WIND SPEED INDICATOR TO MAKE
C     IF-STATEMENTS SHORTER
C        IW=0 FOR METER/SEC
C        IW=1 FOR KNOTS
C
      IW=(IWW-1)/2
      ISPEED=IFF
      IDIR=IDD
C
C
C     CHECK IF DD=99 .I.E. DIRECTION INDETERMINATE
C
C
C     CHECK IF DD INDICATOR IS SENSIBLE
C
      IF(IDIR .GT. 36 .AND. IDIR .LT. 50) RETURN
      IF(IDIR .GT. 86 .AND. IDIR .NE. 99) RETURN
C
C     CHECK IF SHIP OR BUOY, BECAUSE THEN THERE IS NO COUNT NUMBER
C
      IF(ICOUNT .EQ. MINDIC)
     *    THEN
             IF(IDIR .GT. 50 .AND. IDIR .NE. 99) ISPEED=ISPEED+100
             GO TO 100
          END IF
C
C
C     CHECK IF THE DATA IS FROM U.S.S.R.
C
      IF(ICOUNT .EQ. 6310 .OR. ICOUNT .EQ. 2010)
     1        THEN
                   IF(ISPEED .EQ. 77) ISPEED=20
                   IF(ISPEED .EQ. 88) ISPEED=40
                 ELSE
                   IF(IDIR .GT. 50 .AND. IDIR .NE. 99) ISPEED=ISPEED+100
              END IF
C
100   CONTINUE
C
C     IF SPEED IN KNOTS MODIFY TO M/S
C
      IF(IW .EQ. 1) CALL KTOMPSI(ISPEED)
      IF(IDIR .GT. 50 .AND. IDIR .NE. 99) IDIR=IDIR-50
      IF(IDIR .EQ. 99) IDIR=0
C
      IDIR=IDIR*10
C
      RETURN
      END
      SUBROUTINE IC1600(INHEI,LOWEST,MINDIC,OUTHEI)
C
C
C****
C*
C*    NAME     :  IC1600
C*
C*    FUNCTION :  DECODE THE HEIGHT OF LOWEST CLOUDS
C*
C*    INPUT    :  INHEI   - CODE FIGURE FOR THE HEIGHT
C*                LOWEST  - INDICATOR FOR LOWEST CLOUD
C*                          0 = LOW CLOUD
C*                          1 = MEDIUM CLOUD
C*                          2 = HIGH CLOUD
C*                MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTHEI  - DECODED HEIGHT OF LOWEST CLOUDS IN METRES
C*
C*             OUTHEI SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN INHEI
C*
C****
C
      INTEGER OUTHEI
C
      DIMENSION IHEIGHT(12)
C
      DATA IHEIGHT/25,75,150,250,450,800,1250,1750,2250,2600,3500,8000/
C
C***   SET MISSING VALUE
C
      OUTHEI=MINDIC
C
C***   TEST THE VALIDITY OF THE CODE FIGURE
C
      IF(INHEI .EQ. MINDIC .OR. LOWEST .EQ. MINDIC) RETURN
C
      IF(INHEI .LT. 0 .OR. INHEI .GT. 9) RETURN
C
C
C     FOR N = 9 DEFAULT HEIGHTS ARE ALLOCATED DEPENDING ON
C     WHETHER LOWEST CLOUD IS LOW, MEDIUM OR HIGH.
C
      IF ( INHEI.NE.9 ) LOWEST = 0
C
      IND=INHEI + 1 + LOWEST
      OUTHEI=IHEIGHT(IND)
      RETURN
      END
      SUBROUTINE IC1677(ICODE,MINDIC,IHEIGHT)
C
C
C****
C*    NAME     : IC677
C*
C*    FUNCTION :  DECODE THE HEIGHT OF THE BASE OF THE
C*                LOWEST CLOUD  HH OR HSHS
C*
C*    INPUT    :  ICODE    CODE NUMBER FOR THE HEIGHT
C*                MINDIC   MISSING DATA VALUE
C*
C*    OUTPUT   :  IHEIGHT   DECODED HEIGHT IN METRES
C*
C*             IHEIGHT SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN ICODE
C*
C****
C
      DIMENSION IHSHS(9)
C
      DATA IHSHS/25,75,150,250,450,800,1250,1750,2250/
C
C
C***  SET MISSING VALUE
C
      IHEIGHT=MINDIC
C
      IF(ICODE .LT. 0 .OR. ICODE .GT. 98) RETURN
C
C
C
      IF(ICODE .EQ. 89) THEN
                 IHEIGHT=22000
                 RETURN
                 END IF
C
      IF(ICODE .LE. 50) THEN
                 IHEIGHT=ICODE*30 
                 IF(IHEIGHT .LT. 0) IHEIGHT=0
                 RETURN
                 END IF
C
      IF(ICODE .GE. 56 .AND. ICODE .LE. 80) THEN
                 IHEIGHT=(ICODE-50)*300 
                 RETURN
                 END IF
C
      IF(ICODE .GE. 81 .AND. ICODE .LE. 88) THEN
                 IHEIGHT=(ICODE-80)*1500+ 9000 
                 RETURN
                 END IF
C
C     CODE VALUE 99 IS NOT USED FOR HH, ONLY FOR HSHS
C     AND FOR TIME BEING 99 IS NOT DECODED AT ALL.
C
      IF(ICODE .GE. 90) THEN
                           IND=ICODE-89
                           IHEIGHT=IHSHS(IND)
                           RETURN
                        END IF
C
C
      END
      SUBROUTINE IC3590(INPRE,NILPRE,MINDIC,OUTPRE)
C
C
C****
C*
C*    NAME     : IC3590
C*
C*    FUNCTION :  DECODE THE AMOUNT OF PRECIPITATION
C*
C*    INPUT    :  INPRE   - PRECIPITATION CODE FIGURE
C*                NILPRE  - INDICATOR FOR 'NIL' PRECIPITATION
C*                MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTPRE  - DECODED PRECIPITATION IN TENTHS OF MM
C*
C*             OUTPRE SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN INPRE
C*
C****
C
      INTEGER OUTPRE
C
C***   SET MISSING VALUE
C
      OUTPRE=MINDIC
C
C***
C*    TEST IF 'NILPRE' ( IR , CODE TABLE 1819 ) INDICATES THAT
C*    PRECIPATION GROUP IS OMITTED BECAUSE RRR = 0 .
C***
C
      IF ( NILPRE.EQ.3 ) OUTPRE = 0
C
C     IR IS FREQUENTLY MISCODED SO AMOUNT OF RRR REPORTED IS ALSO
C     EXAMINED BEFORE RETURNING.
C
      IF ( INPRE.EQ.MINDIC ) RETURN
C
C
      IF(INPRE .LE. 989) THEN
                  OUTPRE=INPRE*10
                  RETURN
                  END IF
C
      IF(INPRE .GE. 990 .AND. INPRE .LE. 999) THEN
                  OUTPRE=INPRE-990
C
C                 Check if trace of rain 26/08/1998
C
                  IF(OUTPRE.EQ.0) OUTPRE=-1 
                  RETURN
                  END IF
C
      RETURN
      END
      SUBROUTINE IC3845(INTEMP,ISIGN,ICOUNT,IREG,MINDIC,OUTTEMP)
C
C****
C*
C*    NAME     : IC3845
C*
C*    FUNCTION :  DECODE THE TEMPERATURE
C*
C*    INPUT    :  INTEMP   TEMPERATURE VALUE
C*                ISIGN    SIGN INDICATOR FOR TEMPERATURE
C*                         0 = NOT NEGATIVE, 1 = NEGATIVE
C*                ICOUNT  COUNTRY NUMBER.
C*                IREG  REGION NUMBER.
C*                MINDIC   MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTTEMP  OUTPUT TEMPERATURE WITH CORRECT SIGN
C*
C*             OUTTEMP IS SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN INTEMP
C*
C****
C
      INTEGER OUTTEMP
C
C***   SET MISSING VALUE
C
      OUTTEMP=MINDIC
C
C
C     TEST FOR MISSING DATA AND VALIDITY OF SIGN
C
      IF(INTEMP .EQ. MINDIC .OR. ISIGN .EQ. MINDIC) RETURN
      IF(ISIGN .LT. 0 .OR. ISIGN .GT. 1) RETURN
C
      OUTTEMP=INTEMP
      IF(ISIGN .EQ. 1) OUTTEMP = -1*OUTTEMP
C
      RETURN
C     TEMPERATURE IS GIVEN IN FAHRENEIT IN CUBA,NICARAGUA AND PANAMA
C     ( REGION 4 COUNTRY NUMBERS 70,170 AND 190 ) AND HAS TO BE
C     CONVERTED TO CELSIUS.
C
C      IF ( IREG.NE.4 ) RETURN
C      IF ( ICOUNT.NE.70.AND.ICOUNT.NE.170.AND.ICOUNT.NE.190) RETURN
C      CALL FTOC2 ( OUTTEMP )
C
C
C      RETURN
      END
      SUBROUTINE IC3931(ITA,ITT,MINDIC,ITEMP)
C
C
C****
C*
C*    NAME     :  IC3931
C*
C*    FUNCTION :  DECODE TEMPERARURE IN TENTHS OF DEGREE
C*
C*    INPUT    :  ITA     - APPROXIMATE TENTHS VALUE AND SIGN BIT
C*             :  ITT     - TENS AND UNIT DIGITS OF TEMPERATURE
C*             :  MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  ITEMP   - DECODED TEMPERATURE
C*
C*
C*   A.HOLOPAINEN  JAN.83
C*
C****
C
C
C
C***   CHECK MISSING VALUES
C
      IF(ITA .EQ. MINDIC .OR. ITT .EQ. MINDIC) RETURN
C
C     CHECK THE RANGE OF ITA
C
      IF(ITA .LT. 0 .OR. ITA .GT. 9) RETURN
C
C     POSITIVE TEMPERATURE
C
      IVA=2*(ITA/2)
      IF(IVA .EQ. ITA) THEN
                    ITEMP = 10 * ITT + ITA
                          RETURN
                       END IF
C
C     NEGATIVE TEMPERATURE
C
      ITEMP = -10 * ITT - ITA
      RETURN
      END
      SUBROUTINE IC4377(INVIS,MINDIC,OUTVIS)
C
C
C****
C*
C*    NAME     :  IC4377
C*
C*    FUNCTION :  DECODE HORIZONTAL VISIBILITY AT SURFACE VV
C*
C*    INPUT    :  INVIS   -VISIBILITY CODE FIGURE
C*                MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTVIS  -DECODED VISIBILITY IN METRES
C*
C*             OUTVIS IS SET TO MISSING DATA VALUE
C*             IF ANY ERRORS FOUND IN INVIS
C*
C****
C
C
C
      INTEGER OUTVIS
      DIMENSION IVISIB(9)
C
      DATA IVISIB /50,200,500,1000,2000,4000,10000,20000,55000/
C
C***   SET OUTVIS TO MISSING VALUE
C
      OUTVIS=MINDIC
C
      IF(INVIS .EQ. MINDIC) RETURN
      IF(INVIS .GE. 51 .AND. INVIS .LE. 55) RETURN
      IF(INVIS .LT. 0 .OR. INVIS .GT. 99) RETURN
C
      IF(INVIS .EQ. 89) THEN
                              OUTVIS=75000
                              RETURN
                           END IF
C
      IF(INVIS .EQ. 90) THEN
                              OUTVIS=25
                              RETURN
                           END IF
C
      IF(INVIS .EQ. 0) THEN
                             OUTVIS=50
                             RETURN
                           END IF
C
C
      IF(INVIS .GE. 1 .AND. INVIS .LE. 50)
     1       THEN
                 OUTVIS=100 * INVIS
                 RETURN
             END IF
C
      IF(INVIS .GE. 56 .AND. INVIS .LE. 80)
     1       THEN
                 OUTVIS=(INVIS - 50) * 1000
                 RETURN
             END IF
C
      IF(INVIS .GE. 81 .AND. INVIS .LE. 88)
     1       THEN
                 OUTVIS=(INVIS - 80) * 5000 + 30000
                 RETURN
             END IF
C
      IF(INVIS .GE. 91 .AND. INVIS .LE. 99)
     1       THEN
                 IND=INVIS-90
                 OUTVIS=IVISIB(IND)
                 RETURN
             END IF
C
C
      RETURN
      END
      SUBROUTINE IC4451(INVS,MINDIC,OUTVS)
C
C
C****
C*
C*    NAME     : IC4451
C*
C*    FUNCTION :  DECODE SHIPS AVERAGE SPEED  VS
C*
C*    INPUT    :  INVS    - SPEED CODE FIGURE
C*                MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTVS   - DECODED SPEED M/S
C*
C*
C*             IF ANY ERRORS FOUND IN INVS
C*
C****
C
      INTEGER OUTVS
C
      DIMENSION ISPEED(10)
C
      DATA ISPEED/0,1,4,7,9,12,14,17,20,22/
C
C***   SET MISSING VALUE
C
      OUTVIS = MINDIC
C
      IF(INVS .LT. 0 .OR. INVS .GT. 9) RETURN
C
      OUTVS=ISPEED(INVS+1)
C
      RETURN
      END
      SUBROUTINE MARDSEN(LAT,LONG,M,IERROR)
C
C
C**** *MARDSEN*
C
C
C     PURPOSE.
C     --------
C
C         CHECK THE LAT&LONG AGAINST MARDSEN SQUARE
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *MARDSEN(LAT,LONG,M,IERROR)*
C
C          INPUT     : LATITUDE  IN HUNDREDTH'S OF DEGREE
C                      LONGITUDE IN HUNDREDTH'S OF DEGREE
C              M     - MARDSEN SQUARE VALUE GIVEN IN REPORT
C
C          OUTPUT   : IERROR  - ERROR INDICATOR
C
C
C
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         *XXXX* *XXXXXXX(XXXX)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C     A. HOLOPAINEN  JUNE -84
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
C
C     ------------------------------------------------------------------
C
C*          1.   CHECK POSITION.
C                ---------------
 100  CONTINUE
C
C
      IERROR = 0
      LOLO = LONG
C
C     DON'T CHECK IF POSITION ON THE LINE BETWEEN TWO (OR FOUR) SQUARES
C
      LAA = 1000*(LAT/1000)
      LOO = 1000*(LONG/1000)
C
      IF(LAA .EQ. LAT) RETURN
      IF(LOO .EQ. LONG) RETURN
C
      LAT = LAT /10
      LONG= LONG/10
C
C
      IF(LAT .GE. 0)
     C   THEN
            LO = IABS(LONG/100)+1
            IF(LOLO .GE. 0) LO = 37 - LO
C
            IF( LAT .LT. 800 )
     C         THEN
                  MMM = (LAT/100)*36 + LO
               ELSE
                  MMM = 900 + LO
               END IF
         END IF
C
C
      IF(LAT .LT. 0)
     C   THEN
            LO = IABS( LONG/100 )
            IF(LOLO .GE. 0) LO = 35 - LO
            MMM = 300 + IABS( LAT/100 )*36 +LO
         END IF
C
      IF(MMM .NE. M) IERROR = 1
C
      RETURN
C
      END
      SUBROUTINE IC3333(IQUADR,LAT,LONG,MINDIC,LAT2,LONG2)
C
C
C****
C*
C*    NAME     :  IC3333
C*
C*    FUNCTION :  DECODE LATITUDE AND LONGITUDE GIVEN IN THE FORM
C*                99LALALA QCL0L0L0L0
C*
C*    INPUT    :  IQUADR   THE QUADRANT OF THE GLOBE (QC)
C*             :  LAT      LATITUDE IN TENTHS OF DEGREE
C*             :  LONG     LONGITUDE IN TENTHS OF DEGREE
C*             :  MINDIC   MISSING DATA VALUE
C*
C*
C*    OUTPUT   :  LAT2   LATITUDE IN HUNDREDTHS OF DEGREE
C*                       SOUTHERN LATITUDE NEGATIVE
C*             :  LONG2: LONGITUDE IN HUNDREDTHS OF DEGREE
C*                       WESTERN LONGITUDE NEGATIVE
C*
C*                LAT2 AND LONG2 ARE SET TO MISSING DATA VALUE IF
C*                ANY ERRORS FOUND IN LAT,LONG OR QUADRANT
C*
C****
C
C
      DIMENSION LATSIGN(4),LONSIGN(4)
C
      DATA LATSIGN/ 1,-1,-1, 1/
      DATA LONSIGN/ 1, 1,-1,-1/
C
C***  SET LAT2 AND LONG2 TO MISSING DATA VALUE
C
      LAT2=MINDIC
      LONG2=MINDIC
C
C
C     THE ARRAYS LATSIGN AND LONSIGN ARE USED TO DETERMINE IF
C     LAT. AND LONG. ARE NEGATIVE OR POSITIVE
C
C     CHECK THAT THE QUADRANT IS CORRECT
C
      IF(IQUADR .NE. 1 .AND. IQUADR .NE. 3 .AND. IQUADR .NE.
     1     5 .AND. IQUADR .NE. 7) RETURN
C
C     CHECK THAT THE LATITUDE AND LONGITUDE ARE SENSIBLE
C
      IF(LAT .LT. 0 .OR. LAT .GT. 900) RETURN
C
      IF(LONG .LT. 0 .OR. LONG .GT. 1800) RETURN
C
C
      IQ=(IQUADR+1)/2
C
      LAT2=10*LAT*LATSIGN(IQ)
      LONG2=10*LONG*LONSIGN(IQ)
C
      RETURN
C
C
      END
      SUBROUTINE STATION(IERR)

C
C**** *STATION*
C
C
C     PURPOSE.
C     --------
C         READ IN STATION LIST AND MAKE LIST OF IMPORTANT STATIONS.
C         ( WMO VOLUMEN A - LIST OF OBSERVING STATIONS)
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *STATION(IERR)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *IMPSTAT*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'combuff.h'
      include 'combase.h'
      character*256 cf
C
C     ------------------------------------------------------------------
C*          1.   READ IN STATION LIST.
C                ---------------------
 100  CONTINUE
C
      i=index(cppbase,' ')
      i=i-1
      
      cf=' '
      cf=cppbase(1:i)//'/dat/station_list.dat'
      i=index(cf,' ')
      i=i-1
c
      OPEN(UNIT=4,IOSTAT=IOS,ERR=300,
     1     FILE=cf(1:i),
     1     STATUS='OLD',
     1     FORM='UNFORMATTED')
C
C
      READ(4) IPARAMS,IPOINTS
C
      CLOSE(4)
C
C
C*           2.  FIND IMPORTANT STATIONS.
C                ------------------------
 200  CONTINUE
C
      CALL IMPSTAT
C
      RETURN
C
 300  CONTINUE
C
      WRITE(*,9901) IOS
 9901 FORMAT(1H ,' ERROR DURING OPENING  STATION FILE , ERROR=',I5)
C
C
      RETURN
      END
      SUBROUTINE IMPSTAT
C
C
C**** *IMPSTAT*
C
C
C     PURPOSE.
C     --------
C
C         DEFINES FROM WMO MASTER FILE THE SATION NUMBERS
C         FOR IMPORTANT STATIONS (ECMWF INTERNAL DEFINOTIONS)
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *IMPSTAT*
C
C             INPUT     : IPARAMS   STATION INFORMATION IN PACKED FORM
C                         IPOINTS   NUMBER OF STATION / WMO BLOCK
C
C             OUTPUT    : IMPSTA    THE NUMBERS OF IMPORTATNT SATIONS
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
      INCLUDE 'combuff.h'
C
C
C     ------------------------------------------------------------------
C*          1.   FIND IMPORTANT STATIONS.
C                ------------------------
C
      J = 0
      K = 1
C
C
            DO 300 I = 1,26000,2
C
            IF(IPARAMS(I) .EQ. MINDIC) GO TO 400
            IF(I .LT. IPOINTS(K+1)) GO TO 200
C
100         K = K + 1
            IF(K .GE. 99) GO TO 400
            IF(IPOINTS(K) .EQ. IPOINTS(K+1)) GO TO 100
C
200         CONTINUE
C
            CALL GBYTE(IPARAMS(I+1),ITEMP,28,1)
            CALL GBYTE(IPARAMS(I+1),IBIT ,25,1)
C
           IF(ITEMP .EQ. 1 .AND. IBIT .EQ. 1)
     C         THEN
                  CALL GBYTE(IPARAMS(I),III,0,10)
                  ISTA= 1000*K+ III
                  IF(J .NE. 0)
     C               THEN
                        DO 250 N=1,J
                        IF(ISTA .EQ. IMPSTA(N)) GO TO 300
250                     CONTINUE
                     END IF
                  J = J + 1
                  IF(J.GT.4000) THEN
                     PRINT*,'DIMENSION IF IMPSTA TOO SMALL'
                     GO TO 400
                  END IF
C
                  IMPSTA(J) = ISTA
               END IF
C
300        CONTINUE
C
C
400   CONTINUE
C
500   CONTINUE
C
      RETURN
      END
      
      SUBROUTINE LOCSTAT(IWIND,IRC)
C
C**** *LOCSTAT*
C
C
C     PURPOSE.
C     --------
C
C         EXTRACT PARTICULARS OF WMO OBSERVING STATIONS AND
C         PUT IN DECODED REPORT HEADER.
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *LOCSTAT(IWIND,IRC)*
C
C          INPUT    : ARGUMENTS NOT USED ON INPUT .
C
C                     KINT(4) - WMO STATION NUMBER IN INTEGER.
C                     KDEC(4) - INTEGER DENOTING OBSERVATION TYPE.
C
C          OUTPUT   : KDEC(5) - LATITUDE IN HUNDREDTHS OF DEGREES ,
C                               NORTH + , SOUTH - .
C                     KDEC(6) - LONGITUDE IN HUNDREDTHS OF DEGREES ,
C                               EAST + , WEST - .
C                     KDEC(8) - STATION PRESSURE ELEVATION (H/P) OR IF
C                               NONE EXISTS STATION GROUND ELEVATION (H/A).
C                               IF NEITHER EXIST MINDIC IS RETURNED . VALUE
C                               IS INTEGER IN METRES.
C
C                     KDEC(15) - IMPORTANT STATION OR GOOD QUALITY STATION
C                                FLAG BITS SET IN THIS WORD.
C
C                     KDEC(16) - WMO COUNTRY NUMBER , EXCLUDING FIRST 2
C                                DIGITS ( REGION NUMBER ) . INTEGER.
C                     KDEC(17) - WMO REGION NUMBER , INTEGER.
C
C                     KDEC(23) - PRESSURE LEVEL INDICATOR , INTEGER.
C                            0 = SEA LEVEL
C                            1 = STATION LEVEL
C                            2 = 850 HPA
C                            3 = 700 HPA
C                            4 = 500 HPA
C                            5 = 1000 GPM
C                            6 = 2000 GPM
C                            7 = 3000 GPM
C                            8 = 4000 GPM
C                            9 = 900 HPA
C
C                     IRC - INTEGER RETURN CODE
C                            0 = NO ERROR
C                            1 = STATION NUMBER NOT IN DIRECTORY
C                            2 = INVALID STATION NUMBER
C                            3 = INVALID OBSERVATION TYPE
C
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *GBYTE(KS,KD,KBPT,KSI)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC
C
C     MODIFICATIONS.
C     --------------
C
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
      INCLUDE 'combuff.h'
C
C     ------------------------------------------------------------------
C*          1.   LOCATE STATION.                   .
C                ---------------
 100  CONTINUE
C
C     CLEAR ERROR RETURN INDICATOR
C
      IRC = 0
C
C     INITIALIZE TYPE OF STATION
C
      ITYPE = 0
C
C     'ITYPE' IS SET TO 4 FOR SYNOP , 2 FOR PILOT AND 1 FOR TEMP.
C
      IF ( KDEC(4).EQ.11.OR.KDEC(4).EQ.14 ) ITYPE = 4
      IF ( KDEC(4).EQ.32 ) ITYPE = 2
      IF ( KDEC(4).EQ.35 ) ITYPE = 1
C
      IF ( ITYPE.EQ.0 ) THEN
                            IRC = 3
                            RETURN
                        END IF
C
C     CHECK VALIDITY OF STATION NUMBER
C
      IF ( KINT(4).LE.1000.OR.KINT(4).GT.99999 )
     C                       THEN
                                 IRC = 2
                                 RETURN
                             END IF
C
C
C     LOCATE STARTING POINT IN ARRAY 'IPARAMS' OF THE WMO BLOCK
C     OF THE STATION.
C
C     EXTRACT WMO BLOCK NUMBER
C
      II = KINT(4) / 1000
C
C     WORD 'II' OF 'IPOINTS' SHOWS WHERE THE ENTRIES FOR BLOCK 'II'
C     START IN 'IPARAMS'.
C
      IND1 = IPOINTS(II)
      IND2 = IPOINTS(II+1)-3
C
C     STARTING AT THIS WORD A SEQUENTIAL SEARCH IS MADE FOR AN ENTRY
C     FOR THE REQUIRED STATION NUMBER ( III ) >
C
      III = KINT(4) - ( II * 1000 )
c
      if(ii.eq.12.and.iii.eq.851) then
         jjjj=5
      end if
C
C     FOR ONE STATION ENTRY 3 WORDS ARE USED
C
      DO 101 I=IND1,IND2,3
         CALL GBYTE(IPARAMS(I),ISTN,0,10)
         IF(ISTN.EQ.III) GO TO 200
  101 CONTINUE
C
C     STATION NUMBER NOT FOUND
C
      IRC = 1
      RETURN
C
C
C     -----------------------------------------------------------------
C*             2.  EXTRACT REQUIRED PARAMETERS FROM 1ST WORD ENTRY.
C                  -----------------------------------------------
 200  CONTINUE
C
C     PRESSURE LEVEL CODE FIGURE
C
      IF(ITYPE .EQ. 4)
     C   CALL GBYTE(IPARAMS(I),KDEC(23),16,4)
c     print*,ii,iii,kdec(23)
C
C
C     WIND SPEED UNIT INDICATOR
C
      CALL GBYTE(IPARAMS(I),IWIND,20,1)
C
C
C     STATION ELEVATION
C
      CALL GBYTE(IPARAMS(I),KDEC(8),24,14)
      IF ( KDEC(8).GT.9999 ) KDEC(8) = KDEC(8)-16383
      IF ( KDEC(8).EQ.9999 ) KDEC(8) = MINDIC
      CALL GBYTE(IPARAMS(I+1),ISGN,6,2)
      IF(KDEC(8).NE.MINDIC.AND.ISGN.EQ.1) KDEC(8)=-KDEC(8)
C
C     LONGITUDE
C
      CALL GBYTE(IPARAMS(I+1),KDEC(6),8,16)
      IF ( KDEC(6).EQ.65535 ) KDEC(6) = MINDIC
      IF ( KDEC(6).NE.MINDIC.AND.KDEC(6).GT.18000)
     C      KDEC(6) = KDEC(6) - 36000

C
C
C     LATITUDE
C
      CALL GBYTE(IPARAMS(I+1),KDEC(5),24,14)
      IF ( KDEC(5).EQ.16383 ) KDEC(5) = MINDIC
      CALL GBYTE(IPARAMS(I+2),ISGN,6,1)
      IF(ISGN.EQ.1.AND.KDEC(5).NE.MINDIC)
     C                KDEC(5) = - KDEC(5)

C
C
C     WMO REGION NUMBER
C
      CALL GBYTE(IPARAMS(I+2),KDEC(17),8,3)
      IF ( KDEC(17).EQ.0 ) KDEC(17) = 8
C
C
C     WMO COUNTRY NUMBER ( LAST 3 DIGITS )
C
      CALL GBYTE(IPARAMS(I+2),KDEC(16),11,10)
C
C
C     IMPORTANT STATION AND GOOD QUALITY FLAGS.
C
      CALL GBYTE(IPARAMS(I+2),ISGQ,24,2)
      KDEC(15) = IOR(KDEC(15),ISGQ)
C
C
C     CHECK THAT PARAMETERS ARE VALID FOR OBSERVATION TYPE REQUESTED.
C     SOME STATIONS HAVE MORE THAN 1 ENTRY , DEPENDING ON TYPE OF
C     OBSERVATION.
C
      IF(ITYPE.EQ.1) ISKIP=26
      IF(ITYPE.EQ.2) ISKIP=27
      IF(ITYPE.EQ.4) ISKIP=28
C
      CALL GBYTE(IPARAMS(I+2),ITP,ISKIP,1)
      IF (  ITP.NE.0 ) RETURN
C
C     PARAMETERS NOT CORRECT FOR CODE TYPE , SO USE NEXT ENTRY
C     IF IT EXIST
C
      I = I + 3
C
      CALL GBYTE(IPARAMS(I),ISTN,0,10)
      IF(ISTN.EQ.III) GO TO 200
C
C     RETAIN ALREADY EXTRACTED PARAMETERS
C     THAT MEAN THAT STATION TYPE DOES NOT CORRESPOND TO THE MESSAGE
C     RECEIVED. 
C     
      RETURN
C
C
      END
      SUBROUTINE EXTVAL ( I,N,IVAL)
C
C
C**** *EXTVAL*
C
C
C     PURPOSE.
C     --------
C
C         EXTRACTS N FIGURES FROM ARRAY 'KCHAR' , STARTING AT
C         WORD I , CONVERTS CHARACTERS TO INTEGER AND PLACES
C         IN IVAL
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *EXTVAL(I,N,IVAL)*
C
C         INPUT     : I - POINTS TO FIRST CHARACTER TO BE EXTRACTED.
C                     N - NUMBER OF CCITT NO. 5 CHARACTERS TO BE EXTRACTE
C
C         OUTPUT    : IVAL - INTEGER VALUE
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
C
      DIMENSION ILET(11),IFIG(11)
C
      DATA (ILET(J),J=1,11) /
C        E    O,   P,   Q,   R,   T,   U,   W,   X,   Y,   I.
     C  69,  79,  80,  81,  82,  84,  85,  87,  88,  89,  73 /
C
      DATA (IFIG(J),J=1,11) /
C        3    9,   0,   1,   4,   5,   7,   2,   /,   6,   8.
     C  51,  57,  48,  49,  52,  53,  55,  50,  47,  54,  56 /
C
C     ------------------------------------------------------------------
C
C*          1.   EXTRACT N FIGURES FROM KCHAR ARRAY.
C                -----------------------------------
 100  CONTINUE
C
      IAC = 0
      IA = IABS(I)
      IB = IA + N - 1
C
      DO 101 J=IA,IB
C
C
C     STORE KCHAR(J) SO THAT IT WONT BE ALTERED IN THE SUBROUTINE
C
      KTEMP=KCHAR(J)
      KAR = IAND(KCHAR(J) , 127)
C
C          CHECK FOR SPACE CHARACTER .
C
           IF ( KAR.EQ.32 ) THEN
                                      IVAL = MINDIC
                                      RETURN
                                 END IF
C
C          CHECK FOR / CHARACTER .
C
           IF ( KAR.EQ.47 ) THEN
                                     IVAL = MINDIC
                                     RETURN
                                  END IF
C
C          IF LETTER ENCOUNTERED CONVERT TO FIGURE USING THE
C          CCITT NO.2 LETTER/FIGURE RELATIONSHIP.
C
           IF ( KAR.LT.48.OR.KAR.GT.57 )
     C                      THEN
                                  DO 102 JA=1,11
                                       IF ( KAR.EQ.ILET(JA))
     C                                       KAR = IFIG(JA)
  102                             CONTINUE
                           END IF
C
           IF ( KAR.GE.48.AND.KAR.LE.57 )
     C                             THEN
                                    IAC = (IAC + (IAND(KAR,15)))*10
                                   ELSE
                                      IVAL = MINDIC
                                      RETURN
                                   END IF
C
C
C
      KCHAR(J)=KTEMP
C
  101 CONTINUE
C
      IVAL = IAC / 10
C
C
      RETURN
      END
      SUBROUTINE PRESEP ( I,J,*)
C
C
C
C*****
C*
C*    NAME      : LETFIG
C*
C*    FUNCTION  : IF K IS NOT FIGURE CONVERT IT USING THE
C*                CCITT NO.2 LETTER/FIGURE RELATION SHIP.
C*
C*    INPUT     : K - KHARACTER VALUE TO BE CONVERTED
C*
C*    OUTPUT    : K - CONVERTED TO FIGURE IF IT WAS EITHER
C*                    E,O,P,Q,R,T,U,W,X,Y OR I, OTHERWISE
C*                    K REMAINS UNCHANGED.
C*
C*****
C
C
C
      DIMENSION ILET(11),IFIG(11)
C
      DATA (ILET(J),J=1,11) /
C        E    O,   P,   Q,   R,   T,   U,   W,   X,   Y,   I.
     C  69,  79,  80,  81,  82,  84,  85,  87,  88,  89,  73 /
C
      DATA (IFIG(J),J=1,11) /
C        3    9,   0,   1,   4,   5,   7,   2,   /,   6,   8.
     C  51,  57,  48,  49,  52,  53,  55,  50,  47,  54,  56 /
C
C
C
C          IF LETTER ENCOUNTERED CONVERT TO FIGURE USING THE
C          CCITT NO.2 LETTER/FIGURE RELATIONSHIP.
C
           IF ( K.LT.48.OR.K.GT.57 )
     C                      THEN
                                  DO 100 JA=1,11
                                       IF ( K.EQ.ILET(JA))
     C                                       K = IFIG(JA)
  100                             CONTINUE
                           END IF
C
C
      RETURN
      END
      SUBROUTINE DDFFF(IDD,IFF,IWW,ICOUNT,MINDIC,IDIR,ISPEED)
C
C
C**** *DDFFF*
C
C
C     PURPOSE.
C
C
C         DECODE WIND DIRECTION AND SPEED
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *DDFFF(IDD,IFF,IWW,ICOUNT,MINDIC,IDIR,ISPEED)*
C
C          INPUT    :  IDD     - WIND DIRECTION IN TENS OF DEGREE
C                   :  IFF     - WIND SPEEDIN METERS/SEC OR KNOTS
C                   :  IWW     - WIND SPEED INDICATOR (1 FOR KNOTS)
C                   :  ICOUNT  - COUNTRY NUMBER
C                   :  MINDIC  - MISSING DATA VALUE
C
C          OUTPUT   :  IDIR    - DECODED WIND DIRECTION
C                      ISPEED  - DECODED WIND SPEED
C
C              IDIR AND ISPEED ARE SET TO MISSING VALUE IF
C              ANY ERRORS FOUND IN IDD, IFF OR IWW
C
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         *XXXX* *XXXXXXX(XXXX)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          A.HOLOPAINEN  JAN.83
C
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
C
C     ------------------------------------------------------------------
C*          1.   DECODE WIND DIRECTION AND SPEED.
C                --------------------------------
 100  CONTINUE
C
C      CHECK MISSING DATA INDICATOR
C
      IF(IDD .EQ. MINDIC .OR. IFF .EQ. MINDIC) RETURN
C
C     CHECK IF IDD ID FEASABLE
C
      IF(IDD .LT. 0 .OR. IDD .GT. 36) RETURN
C
      ISPEED=IFF
      IDIR=IDD
C
C
C     CHECK IF DD=99 .I.E. DIRECTION INDETERMINATE
C
C
C     CHECK IF DD INDICATOR IS SENSIBLE
C
      IF(IDIR .GT. 36 .AND. IDIR .LT. 50) RETURN
      IF(IDIR .GT. 86 .AND. IDIR .NE. 99) RETURN
C
C
C
C
      IF(IDIR .GT. 50 .AND. IDIR .NE. 99) IDIR=IDIR-50
      IF(IDIR .EQ. 99) IDIR=0
C
      IDIR=IDIR*10
C
      IF(ISPEED .GE. 500) THEN
                             ISPEED=ISPEED-500
                             IDIR=IDIR+5
                            END IF
C
C
C     IF SPEED IN KNOTS MODIFY IT TO M/S
C
      IF(IWW .EQ. 1) CALL KTOMPSI(ISPEED)
C
      RETURN
      END
      SUBROUTINE PREPRT(I,J,*)
C
C
C**** *PREPRT*
C
C
C     PURPOSE.
C     --------
C
C         SCANS BULLETIN IN 'KCHAR' FOR PREVIOUS CHARACTER WHICH
C         IS NOT 'SOH' , 'CR' , 'LF' , 'SPACE' OR 'GS' .
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PREPRT(I,J,*)*
C
C         INPUT     : I - SCAN STARTS AT WORD I.
C                     J - SCAN STOPS AT WORD J .
C
C         OUTPUT    : I - POSITION OF REQUIRED CHARACTER.
C                         IF CHARACTER NOT FOUND THE CONROL
C                         RETURNS TO ALTERNATIVE RETURN POINT *
C
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         *XXXX* *XXXXXXX(XXXX)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
C
C     ------------------------------------------------------------------
C*          1.   SCAN BULLETIN.
C                --------------
 100  CONTINUE
C
C     'SOH' = 1 , 'LF' = 10 , 'CR' = 13 , SPACE = 32 , 'GS' = 29.
C
      I = IABS(I)
      K = I
      DO 101 I=K,J,-1
         IF(I .LE. J) RETURN 1
         KAR = IAND(KCHAR(I),127)
         IF ( KAR.NE.1.AND.KAR.NE.10.AND.KAR.NE.13.
     C            AND.KAR.NE.32.AND.KAR.NE.29) RETURN
  101 CONTINUE
C
      RETURN 1
      END
      SUBROUTINE NEXSEP2 ( I,J,*)
C
C
C**** *NEXSEP2*
C
C
C     PURPOSE.
C     --------
C
C         LOCATE THE NEXT GROUP BY FINDING THE NEXT
C         CHARACTER WHICH IS NOT 'CR' OR 'LF' OR 'SPACE'.
C         'CR' OR 'LF' OR 'SPACE'
C
C         INPUT     : I - SCAN STARTS AT WORD 'I' OF 'KCHAR' .
C                     J - SCAN ENDS AT WORD 'J' OF 'KCHAR' .
C
C         OUTPUT    : I - POSITION OF NEXT 'CR' OR 'LF' OR 'SPACE' CHARACTER
C                         IF NO CHARACTER FOUND THE CONTROL RETURN TO
C                         ALTERNATIVE RETURN POINT *
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXSEP2(I,J,*)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
C
C     ------------------------------------------------------------------
C*          1.  SCAN BULLETIN.                     .
C                -------------
 100  CONTINUE
C
C
C     'CR' = 13 , 'LF' = 10 , 'SPACE' = 32.
C
      I=IABS(I)
      K = I
      DO 101 I=K,J
         IF(I .GE. J) RETURN 1
         KAR = IAND(KCHAR(I), 127)
         IF(KAR .EQ. 13 .OR. KAR .EQ. 10 .OR. KAR .EQ. 32) RETURN
  101 CONTINUE
C
      RETURN 1
      END
      SUBROUTINE NEXPRT2(I,J,*)
C
C
C**** *NEXPRT2*
C
C
C     PURPOSE.
C     --------
C
C         SCANS BULLETIN IN 'KCHAR' FOR NEXT CHARACTER WHICH
C         IS NOT 'SOH' , 'CR' , 'LF' , 'SPACE' OR 'GS' .
C
C         INPUT     : I - SCAN STARTS AT WORD I.
C                     J - SCAN STOPS AT WORD J .
C
C         OUTPUT    : I - POSITION OF REQUIRED CHARACTER.
C                         IF CHARACTER NOT FOUND THE CONROL
C                         RETURNS TO ALTERNATIVE RETURN POINT *
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXPRT2(I,J,*)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
C
C     ------------------------------------------------------------------
C
C*          1.   SCAN BULLETIN.                    .
C                --------------
 100  CONTINUE
C
C     'SOH' = 1 , 'LF' = 10 , 'CR' = 13 , SPACE = 32 , 'GS' = 29.
C
      I = IABS(I)
      K = I
      DO 101 I=K,J
         IF(I .GE. J) RETURN 1
         KAR = IAND(KCHAR(I),127)
         IF ( KAR.NE.1.AND.KAR.NE.10.AND.KAR.NE.13.
     C            AND.KAR.NE.32.AND.KAR.NE.29) RETURN
  101 CONTINUE
C
      RETURN 1
      END
      SUBROUTINE NEXTEQ ( I,J )
C
C
C
C**** *EXTINT*
C
C
C     PURPOSE.
C     --------
C         EXTRACTS N FIGURES FROM ARRAY 'KCHAR' , STARTING AT
C         WORD I , CONVERTS CHARACTERS TO INTEGER AND PLACES
C         IN WORD K OF 'KINT' .
C
C         INPUT     : I - POINTS TO FIRST CHARACTER TO BE EXTRACTED.
C                     N - NUMBER OF CCITT NO. 5 CHARACTERS TO BE EXTRACTED.
C
C         OUTPUT    : I - POINTS TO CHARACTER AFTER THE LAST ONE EXTRACTED.
C                         MADE NEGATIVE IF A 'SEPARATOR' IS FOUND IN THE
C                         CHARACTERS BEING EXTRACTED.
C                         IF NEGATIVE , THE ABSOLUTE VALUE IS POSITION OF
C                         'SEPARATOR' ENCOUNTERED .
C                     K - INTEGER VALUE IN WORD K OF 'KINT'. MISSING DATA
C                         VALUE INSERTED IF '/' OR NON DIGIT ENCOUNTERED.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *EXTINT ( I,N,K )*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         *XXXX* *XXXXXXX(XXXX)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          J. HENNESSY
C
C     MODIFICATIONS.
C     --------------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
C
      DIMENSION ILET(11),IFIG(11)
C
      DATA (ILET(J),J=1,11) /
C        E    O,   P,   Q,   R,   T,   U,   W,   X,   Y,   I.
     C  69,  79,  80,  81,  82,  84,  85,  87,  88,  89,  73 /
C
      DATA (IFIG(J),J=1,11) /
C        3    9,   0,   1,   4,   5,   7,   2,   /,   6,   8.
     C  51,  57,  48,  49,  52,  53,  55,  50,  47,  54,  56 /
C
C
C     ------------------------------------------------------------------
C
C*          1.   EXTRACT N FIGURES.
C                ------------------
 100  CONTINUE
C
      IAC = 0
      IA = IABS(I)
      IB = IA + N - 1
C
C*           1.1 STORE KCHAR(J) SO THAT IT WONT BE ALTERED IN THE SUBROUTINE.
C                ------------------------------------------------------------
 110  CONTINUE
C
      DO 111 J=IA,IB
C
      KTEMP=KCHAR(J)
      KTEMP=KCHAR(J)
      KAR = IAND(KCHAR(J) , 127)
C
C
C          CHECK FOR SPACE,LINE FEED AND CARRIAGE RETURN CHARACTER .
C
           IF ( KAR .EQ. 32 .OR. KAR .EQ. 10 .OR. KAR .EQ. 13)
     C                           THEN
                                      I = - J
                                      KINT(K) = MINDIC
                                      RETURN
                                 END IF
C
C          CHECK FOR / CHARACTER .
C
           IF ( KAR.EQ.47 ) THEN
                                     I = IB + 1
                                     KINT(K) = MINDIC
                                     RETURN
                                  END IF
C
C          IF LETTER ENCOUNTERED CONVERT TO FIGURE USING THE
C          CCITT NO.2 LETTER/FIGURE RELATIONSHIP.
C
           IF ( KAR .LT. 48 .OR. KAR .GT. 57 )
     C                      THEN
                                  DO 112 JA=1,11
                                       IF ( KAR .EQ. ILET(JA))
     C                                       KAR = IFIG(JA)
  112                             CONTINUE
                           END IF
C
           IF ( KAR .GE. 48 .AND. KAR .LE. 57 )
     C                             THEN
                                    IAC = (IAC + (IAND(KAR,15)))*10
                                   ELSE
                                      KINT(K) = MINDIC
                                      I = IB + 1
                                      RETURN
                                   END IF
C
C
C
      KCHAR(J)=KTEMP
C
  111 CONTINUE
C
      KINT(K) = IAC / 10
      I = J
C
C
      RETURN
      END
      SUBROUTINE NEXTPRT ( I,J )
C
C
C**** *NEXTPRT*
C
C
C     PURPOSE.
C     --------
C         SCANS BULLETIN IN 'KCHAR' FOR NEXT CHARACTER WHICH
C         IS NOT 'SOH' , 'CR' , 'LF' , 'SPACE' OR 'GS' .
C
C         INPUT     : I - SCAN STARTS AT WORD I.
C                     J - SCAN STOPS AT WORD J .
C
C         OUTPUT    : I - POSITION OF REQUIRED CHARACTER. I > J INDICATES
C                         CHARACTER NOT FOUND.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTPRT(I,J)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
C
C     ------------------------------------------------------------------
C
C*          1.   SCAN BULLETIN.
C                --------------
 100  CONTINUE
C
C     'SOH' = 1 , 'LF' = 10 , 'CR' = 13 , SPACE = 32 , 'GS' = 29.
C
      I = IABS(I)
      K = I
      DO 101 I=K,J
         KAR = IAND(KCHAR(I),127)
         IF ( KAR.NE.1.AND.KAR.NE.10.AND.KAR.NE.13.
     C            AND.KAR.NE.32.AND.KAR.NE.29) RETURN
  101 CONTINUE
C
      RETURN
      END
      SUBROUTINE NEXTSEP ( I,J )
C
C
C**** *NEXTSEP*
C
C
C     PURPOSE.
C     --------
C         LOCATE THE NEXT GROUP BY FINDING THE NEXT
C         CHARACTER WHICH IS NOT 'CR' OR 'LF' OR 'SPACE'.
C                        'CR' OR 'LF' OR 'SPACE'
C
C         INPUT     : I - SCAN STARTS AT WORD 'I' OF 'KCHAR' .
C                     J - SCAN ENDS AT WORD 'J' OF 'KCHAR' .
C
C         OUTPUT    : I - POSITION OF NEXT 'CR' OR 'LF' OR 'SPACE' CHARACTER
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTSEP(I,J)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
C
C     ------------------------------------------------------------------
C
C*          1.   SCAN BULLETIN.
C                --------------
 100  CONTINUE
C
C     'CR' = 13 , 'LF' = 10 , 'SPACE' = 32.
C
      I=IABS(I)
      K = I
      DO 101 I=K,J
         KAR = IAND(KCHAR(I), 127)
         IF(KAR .EQ. 13 .OR. KAR .EQ. 10 .OR. KAR .EQ. 32) RETURN
  101 CONTINUE
C
      RETURN
      END
      SUBROUTINE NEXTEND ( I,J )
C
C
C**** *NEXTEND*
C
C
C     PURPOSE.
C     --------
C         FUNCTION  : LOCATE NEXT OCCURRENCE OF EITHER 'CR' OR 'LF'
C
C         INPUT     : I - SCAN STARTS AT WORD 'I' OF 'KCHAR' .
C                     J - SCAN ENDS AT WORD 'J' OF 'KCHAR' .
C
C         OUTPUT    : I - POSITION OF NEXT 'CR' OR 'LF' CHARACTER.
C                     I > J INDICATES NO CHARACTER FOUND.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTEND(I,J)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
C
C     ------------------------------------------------------------------
C
C*          1.   SCAN BULLETIN.
C                --------------
 100  CONTINUE
C
C     'CR' = 13 , 'LF' = 10 .
C
      I=IABS(I)
      K = I
      DO 101 I=K,J
         KAR = IAND(KCHAR(I) , 127)
         IF ( KAR .EQ. 13 .OR. KAR .EQ. 10 ) RETURN
  101 CONTINUE
C
      RETURN
      END
      SUBROUTINE PRTBULL ( I,M )
C
C
C**** *PRTBULL*
C
C
C     PURPOSE.
C     --------
C         PRINTS BULLETIN IN ARRAY 'KCHAR'
C
C         INPUT      : BULLETIN IN 'KCHAR' .
C                      I - PRINT STARTS AT CHARACTER I
C                      M - PRINT ENDS AT CHARACTER M .
C
C         OUTPUT     : BULLETIN IS PRINTED . ARRAY 'KCHAR' AND POINTERS
C                      UNCHANGED.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PRTBULL(I,M)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *NEXTPRT(I,J)*
C         *CALL* *NEXTEND(I,J)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
C      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
C
      DIMENSION LINE(80)
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT BULLETIN.
C                ---------------
 100  CONTINUE
C
      IP = I
      J = M
C
C*          1.1  SET OUTPUT LINE TO ALL SPACES .
C                -------------------------------
 110  CONTINUE
C
      K = 80
      DO 111 N=1,K
             LINE(N) = 32
  111 CONTINUE
C
C     LOCATE START AND END OF NEXT LINE OF CHARACTERS ( IF ANY ).
C
      CALL NEXTPRT ( IP,J )
      IF ( IP.GT.J ) RETURN
      JP = IP
      CALL NEXTEND ( JP,J )
      K = JP - IP
      IF(K.GT.80) K=80
C
C     INSERT IN OUTPUT LINE AND SUPPRESS PARITY BIT.
C
      DO 112 N =1,K
             LINE(N) = IAND(KCHAR(IP),127)
             IP = IP + 1
  112 CONTINUE
C
      WRITE ( *,9900) (LINE(N),N=1,K)
C
C     GET NEXT LINE
C
      GO TO 110
C
 9900 FORMAT (1H ,80A1)
C
      END
      SUBROUTINE INITVAR ( IERR )
C
C
C
C**** *NEXTFIG*
C
C
C     PURPOSE.
C     --------
C         LOCATE FIRST WORD CONTAINING A FIGURE IN ARRAY
C         'KCHAR' BETWEEN WORD 'I' AND WORD 'K' .
C
C         INPUT     : BULLETIN IN 'KCHAR' , 1 CHARACTER PER WORD.
C
C         OUTPUT    : I = REQUIRED LOCATION . I > K MEANS NO FIGURE FOUND.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTFIG(I,K)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
C
C     ------------------------------------------------------------------
C
C*          1.   FIND POINTER TO NEXT FIGURE.
C                ----------------------------
 100  CONTINUE
C
C
      I = IABS(I)
      J = I
      DO 101 I=J,K
         KAR = IAND(KCHAR(I) , 127)
         IF ( KAR .GE. 48 .AND. KAR .LE. 57 ) RETURN
  101 CONTINUE
C
      RETURN
      END
      SUBROUTINE NEXTLET ( I,K )
C
C
C**** *NEXTLET*
C
C
C     PURPOSE.
C     --------
C
C         LOCATE FIRST WORD CONTAINING A LETTER IN ARRAY
C         'KCHAR' BETWEEN WORD 'I' AND WORD 'K' .
C
C         INPUT     : BULLETIN IN 'KCHAR' , 1 CHARACTER PER WORD.
C
C         OUTPUT    : I = REQUIRED LOCATION . I > K MEANS NO LETTER FOUND.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTLET(I,K)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
C
C     ------------------------------------------------------------------
C
C*          1.   FIND POINTER TO NEXT LETTER.
C                ----------------------------
 100  CONTINUE
C
C
      I = IABS(I)
      J = I
      DO 110 I=J,K
         KAR = IAND(KCHAR(I) , 127)
         IF ( KAR .GE. 65 .AND. KAR .LE. 90 ) RETURN
  110 CONTINUE
C
      RETURN
      END
      SUBROUTINE EXTGRP ( I,N1,N2,N3,N4,N5,N,IRET )
C
C
C
C**** *ERRFILE*
C
C
C     PURPOSE.
C     --------
C         WRITE PROBLEM BULLETIN TO THE ERROR FILE TOGATHER WITH
C         KEY.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *ERRFILE(IHEAD,IERR)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/08/88.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
      character*256 cf
C
      CHARACTER*1 Y63
      CHARACTER*80 YOUT,YOUTA
C     CHARACTER*16384 YCHAR
      CHARACTER*3 YSPEC
C
      YSPEC=CHAR(13)//CHAR(13)//CHAR(10)
      Y63  =CHAR(63)
C     ------------------------------------------------------------------
C
C*          1.   WRITE BULLETIN TO THE ERROR FILE.
C                ---------------------------------
 100  CONTINUE
C
      J1=1
C
C*          1.1  OPEN ERROR FILE AT THE BEGINNING OF THE PROCESS.
C                -------------------------------------------------
 110  CONTINUE
C
      cf=' '
      cf='/home/ma/maa/err/SYNO.error.dat'
      i=index(cf,' ')
      i=i-1
c
      OPEN(UNIT=10,IOSTAT=IOS,ERR=400,
     C     FILE=cf(1:i),
     C     STATUS='UNKNOWN',
     C     RECL=80                      )
C
C     -----------------------------------------------------------------
C*          2. INITIALIZE POINTERS.
C              --------------------
 200  CONTINUE
C
      IST=1
      IEND=1
C
C
C     -----------------------------------------------------------------
C*          3. WRITE BULLETIN INTO ERROR FILE AND MARK ERROR.
C              ----------------------------------------------
 300  CONTINUE
C
      YOUT=' '
      YOUTA=' '
C
      CALL NEXTEND(IEND,ILEN)
      IF(IEND.GT.ILEN) GO TO 500
      CALL NEXTPRT(IEND,ILEN)
C      IF(IEND.GT.ILEN) GO TO 500
      IEND=IEND-1
C
      II=0
C
      DO 301 I=IST,IEND
C
      II=II+1
      IF(KCHAR(I).GT.127) THEN
                             YOUTA(II:II)=Y63
                             ISIGN=1
                          END IF
      YOUT(II:II)=CHAR(IAND(KCHAR(I),127))
C
 301  CONTINUE
C
      IEND=IEND+1
      IST=IEND
C
      WRITE(10,'(A)') YOUT
      IF(ISIGN.EQ.1)  WRITE(10,'(A)') YOUTA
      ISIGN=0
C
C
      GO TO 300
C
 400  CONTINUE
C
      PRINT*,'+++ ERROR DURING OPENNING UNIT 10 +++, IOS=',IOS
C
 500  CONTINUE
C
      CLOSE(10)
C
      RETURN
      END
      SUBROUTINE NEXTVAL ( I,N,K )
C
C
C**** *NEXTVAL*
C
C
C     PURPOSE.
C     --------
C
C         LOCATE THE FIRST WORD CONTAINING THE VALUE 'N' IN
C         ARRAY 'KCHAR' BETWEEN WORD 'I' AND WORD 'K' .
C
C         INPUT    : 'KCHAR' CONTAINS ONE BULLETIN , ONE CHARACTER PER
C                    WORD.
C
C         OUTPUT   : I = REQUIRED LOCATION . I > K MEANS VALUE NOT FOUND.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTVAL(I,N,K)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
C
C     ------------------------------------------------------------------
C
C*          1.   EXTRACT VALUE.
C                --------------
 100  CONTINUE
C
      I = IABS(I)
      J = I
      DO 101 I=J,K
         KAR = IAND(KCHAR(I) , 127)
         IF ( KAR .EQ. N ) RETURN
  101 CONTINUE
C
      RETURN
      END
      SUBROUTINE PRTKDEC(IA,K,J,MINDIC)
C
C
C**** *PRTKDEC*
C
C
C     PURPOSE.
C     --------
C         PRINTS THE DECODED FORMAT ARRAY (KDEC)
C         OF DECODING DATA (PHASE II).
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PRTKEDEC(IA,K,J,MINDIC)*
C
C         INPUT     : IA     - THE 'KDEC' ARRAY
C                      K      - PRINT STARTS AT WORD I.
C                      J      - PRINT STOPS AT WORD J .
C                      MINDIC - MISSING VALUE INDICATOR
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/08/88.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      DIMENSION IA(1)
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT ARRAY 'KDEC'.
C                -------------------
 100  CONTINUE
C
C
C
      LODATA = .TRUE.
      LODOT = .TRUE.
C
      WRITE(*,10000)
10000 FORMAT(1H ,2X,'  DATA IN DECODED FORMAT ( KDEC ) ',/)
C
C
         DO 101 I=K,J,10
         I2 = I+9
C
            DO 102 JJ=I,I2
            IF(IA(JJ) .EQ. MINDIC) GO TO 102
            LODATA = .FALSE.
102         CONTINUE
C
         IF(LODATA) THEN
C
C                      CHECK IF THERE ARE MORE DATA
C
                       DO 103 JJ=I2,J
                       IF(IA(JJ) .EQ. MINDIC) GO TO 103
                       GO TO 104
103                    CONTINUE
                       RETURN
C
104                    CONTINUE
C
                       IF(LODOT) THEN
                                    WRITE(*,20000)
20000                               FORMAT(1H ,3X,'---',/1H ,3X,'---')
                                    LODOT = .FALSE.
                                 END IF
                       GO TO 101
                    END IF
C
         LODOT = .TRUE.
         LODATA = .TRUE.
C
         IF(I .EQ. 1)  WRITE(*,30000) I,(IA(IK),IK=I,I2)
         IF(I .EQ. 11) WRITE(*,40000) I,(IA(IK),IK=I,I2)
         IF(I .GT. 20) WRITE(*,50000) I,(IA(IK),IK=I,I2)
30000    FORMAT(1H ,2X,I4,4X,6(I10,1X),6X,A4,1X,3(I10,1X))
40000    FORMAT(1H ,2X,I4,4X,2(I10,1X),O10,1X,I10,1X,O10,1X,2(I10,1X),
     C          2(I10,1X),I10)
50000    FORMAT(1H ,2X,I4,4X,10(I10,1X))
C
101      CONTINUE
C
C
C
C
C
      RETURN
      END
      SUBROUTINE PRTKINT(IA,K,KL,MINDIC)
C
C
C
C**** *SYNEXP1*
C
C
C     PURPOSE.
C     --------
C         SET UP BUFR EXPANDED FORMAT FOR SYNOP DATA.
C         BASIC REPORT. 
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SYNEXP1(IERR)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *DATUM(I,J,K)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parbuf.h'
      PARAMETER (KDLEN=200,KELEM=600,KELEM1=600,KVALS=40000)
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
      INCLUDE 'compoin.h'
C
      INCLUDE 'comkey.h'
      INCLUDE 'cominit.h'
      INCLUDE 'comstation.h'
      INCLUDE 'comwrt.h'
      INCLUDE 'comwrtc.h'
      INCLUDE 'comsubs.h'
      character*2 csp00,csp03,csp06,csp09,csp12,csp15,csp18,csp21
      character*1 cuat00,cuat06,cuat12,cuat18
      character*32 cstation
      logical first      

C
      CHARACTER*9 CIDENT
      INCLUDE 'comkeyc.h'
C
      REAL*8 RVIND
      DIMENSION KSUP(JSUP)  ,KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION KSEC2(JSEC2),KSEC3(JSEC3),KSEC4(JSEC4)
      DIMENSION KEY(JKEY)
C
      REAL*8 VALUES(KVALS)
      DIMENSION KTDLST(KELEM),KTDEXP(KELEM),KTDLST1(KELEM)
      DIMENSION KDATA(KDLEN), INDX(2)
C
      DIMENSION KBUFR(JBUFL)
C
C        
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS (KELEM)
      CHARACTER*23 CTIME

      CHARACTER*20 STNAME

C     ------------------------------------------------------------------
C*          1.   INCREASE COUNTER OF SUBSETS BY ONE.
C                -----------------------------------
 100  CONTINUE
C
      IF(IERR.NE.0) RETURN
C
      RVIND=1.7D38
      EPS=1.0D-8
      IF(.NOT.OLAST) THEN

C       NSUB=0
        NSUB=NSUB+1
        N   =NSUB
C
        DO J=NSUB,NSUB
        IJ=(J-1)*KELEM
        DO I=1,134
        VALUES(I+IJ)=RVIND
        END DO
        END DO
C
C*          1.1  GET CURRENT DATE (YEAR AND MONTH).
C                ----------------------------------
 110  CONTINUE
C
        CALL DATUM(KDEC(1),IMONTH,IYEAR,IERR)
        IF(IERR.NE.0) RETURN
C
C     ------------------------------------------------------------------
C*          2.   SYNOP - LAND BASED STATION.
C                --------------------------
 200  CONTINUE
C
        IBL=(KINT( 9)-48)*10+KINT(10)-48
        IST=(KINT(11)-48)*100+(KINT(12)-48)*10+KINT(13)-48
        IF(IBL.EQ.2.AND.IST.EQ.418) THEN
         JJJJJ=5
        END IF
c
        IJ=(NSUB-1)*KELEM

        IF(KDEC(20).EQ.1) THEN
C          NIL REPORT
           VALUES(  1+IJ)=IBL
           VALUES(  2+IJ)=IST
C          VALUES(  3+IJ)=NSUB*1000+20
           VALUES( 37+IJ)=4.      
           VALUES( 54+IJ)=0.
C
C          Fill in KDATA
C
           
           KDATA(1)=4
           KDATA(2)=0
C
           KTDLEN=1
           KTDLST(1)=307080
C
           DO I=1,20
           CVALS(NSUB)(I:I)=CHAR(255)
           END DO
       

        ELSEIF(KDEC(4).EQ.11.OR.
     1        KDEC(4).EQ.14    ) THEN
C     -----------------------------------------------------------------
C*          2.1  SYNOP LAND ( MANUAL/AUTOMATIC) STATION.
C                ----------------------------------------
C                LOW ALTITUDE STATION.
C                ---------------------
 210  CONTINUE
C
C
        nstid=((KINT( 9)-48)*10+KINT(10)-48)*1000+
     1      (KINT(11)-48)*100+(KINT(12)-48)*10+KINT(13)-48
        idx=0
        do i=1,nst
         if(nstid.eq.istid(i)) then
          idx=i
          go to 211
         end if
        end do
C                                   Element name                              Unit
 211    if(idx.eq.0) then
         print*,nstid,' not found'
         return
        end if
C 
        m=1
        values(  m+IJ)=(KINT( 9)-48)*10+KINT(10)-48       !  001001  WMO BLOCK NUMBER                          NUMERIC           
        m=m+1
        values(  m+IJ)=(KINT(11)-48)*100+(KINT(12)-48)*10
     1             +KINT(13)-48                      !  001002  WMO STATION NUMBER                        NUMERIC           
        m=m+1
      
        values(  m+IJ)=nsub*1000+20                              !  001015  STATION OR SITE NAME                      CCITTIA5          
        cvals(nsub)=cstation(idx)(1:20)
        IF(kint(15).LE.3) THEN
           m=m+1
           values(  m+IJ)=1.     ! manned                         !  002001  TYPE OF STATION                           CODE TABLE 002001 
        ELSEIF(kint(15).LE.7) THEN
           m=m+1
           values(  m+IJ)=0.     ! automatic                       !  002001  TYPE OF STATION                           CODE TABLE 002001
        ELSE
           m=m+1
           values(  m+IJ)=RVIND
        END IF
        m=m+1
        values(  m+IJ)=float(IYEAR )                      !  004001  YEAR                                      YEAR              
        m=m+1
        values(  m+IJ)=float(IMONTH)                      !  004002  MONTH                                     MONTH             
        m=m+1
        values(  m+IJ)=float(KDEC(1))                     !  004003  DAY                                       DAY               
        m=m+1
        values(  m+IJ)=float(KDEC(2))                     !  004004  HOUR                                      HOUR              
        m=m+1
        values(  m+IJ)=float(KDEC(9))                     !  004005  MINUTE                                    MINUTE            
        m=m+1
        values( m+IJ)=KDEC(5)/100.                       !  005001  LATITUDE (HIGH ACCURACY)                  DEGREE
        m=m+1
        values( m+IJ)=KDEC(6)/100.                       !  006001  LONGITUDE (HIGH ACCURACY)                 DEGREE            
        IF(KDEC(8).NE.MINDIC) THEN
           m=m+1
           if(istha(idx).ne.9999) then
           values( m+IJ)=float(istha(idx))
           else
           values( m+IJ)=rvind
           end if
c          values( m+IJ)=float(KDEC(8))                  !  007030  HEIGHT OF STATION GROUND ABOVE MEAN SEA   M
        ELSE 
           m=m+1
           values( m+IJ)=RVIND
        END IF
        IF(KDEC(8).NE.MINDIC) THEN
           m=m+1
           if(isthp(idx).ne.9999) then
           values( m+IJ)=float(isthp(idx))
           else
           values( m+IJ)=rvind
           end if
c          values( m+IJ)=float(KDEC(8))
        ELSE
           m=m+1
           values( m+IJ)=RVIND                              !  007031  HEIGHT OF BAROMETER ABOVE MEAN SEA LEVEL  M                 
        END IF
        IF(KDEC(34).NE.MINDIC) THEN
           m=m+1
           values( m+IJ)=KDEC(34)*10.                    !  010004  PRESSURE                                  PA
        ELSE
           m=m+1
           values( m+IJ)=RVIND 
        END IF
        IF(KDEC(35).NE.MINDIC) THEN
           m=m+1
           values( m+IJ)=KDEC(35)*10.                    !  010051  PRESSURE REDUCED TO MEAN SEA LEVEL        PA                
        ELSE
           m=m+1
           values( m+IJ)=RVIND
        END IF
        IF(KDEC(39).NE.MINDIC) THEN
           m=m+1
           values( m+IJ)=KDEC(39)*10.                    !  010061  3-HOUR PRESSURE CHANGE                    PA                
        ELSE
           m=m+1
           values( m+IJ)=RVIND
        END IF
        IF(KDEC(38).NE.MINDIC) THEN
           m=m+1
           values( m+IJ)=KDEC(38)                        !  010063  CHARACTERISTIC OF PRESSURE TENDENCY       CODE TABLE 010063 
           IF(KDEC(38).GE.9) values( m+IJ)=15.
        ELSE
           m=m+1
           values( m+IJ)=RVIND
        END IF
        m=m+1
        values( m+IJ)=RVIND                           !  010062  24-HOUR PRESSURE CHANGE                   PA                
        IF(KDEC(36).NE.MINDIC) THEN
           m=m+1
           values( m+IJ)=KDEC(36)*100.                !  007004  PRESSURE                                  PA
        ELSE
           m=m+1
           values( m+IJ)=RVIND          
        END IF
        IF(KDEC(37).NE.MINDIC) THEN
           m=m+1
           values( m+IJ)=KDEC(37)
        ELSE
           m=m+1
           values( m+IJ)=RVIND                           !  010009  GEOPOTENTIAL HEIGHT                       GPM               
        END IF
        m=m+1
        if(RH_tem(idx).ne.99.9) then
        values( m+IJ)=RH_tem(idx)                                 !  007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR   M                 
        else
        values( m+IJ)=rvind
        end if
        IF(KDEC(31).NE.MINDIC) THEN
           m=m+1
           values( m+IJ)=KDEC(31)/10.+273.15             !  012101  TEMPERATURE/DRY-BULB TEMPERATURE          K                 
        ELSE
           m=m+1
           values( m+IJ)=RVIND
        END IF
        IF(KDEC(32).NE.MINDIC) THEN
           m=m+1
           values( m+IJ)=KDEC(32)/10.+273.15             !  012103  DEW-POINT TEMPERATURE                     K                 
        ELSE
           m=m+1
           values( m+IJ)=RVIND
        END IF
        IF(KDEC(33).NE.MINDIC) THEN
           m=m+1
           values( m+IJ)=KDEC(33)                        !  013003  RELATIVE HUMIDITY                         %                 
        ELSE
           m=m+1
           values( m+IJ)=RVIND
        END IF
        m=m+1
        if(RH_vis(idx).ne.99.9) then
        values( m+IJ)=RH_vis(idx)                                 !  007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR   M                 
        else
        values( m+IJ)=rvind
        end if
        IF(KDEC(27).NE.MINDIC) THEN
           m=m+1
           values( m+IJ)=KDEC(27)                        !  020001  HORIZONTAL VISIBILITY                     M                 
        ELSE
           m=m+1
           values( m+IJ)=RVIND
        END IF
        m=m+1
        if(RH_prec(idx).ne.99.9) then
        values( m+IJ)=RH_prec(idx)                                 !  007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR   M                 
        else
        values( m+IJ)=rvind
        end if
        IF(KINT(117).NE.MINDIC) THEN
           m=m+1
           if(KINT(117).eq.9999) then
              values( m+IJ)=-0.1
           else
              values( m+IJ)=KINT(117)/10.                 !  013023  TOTAL PRECIPITATION PAST 24 HOURS         KG/M**2           
           end if
        ELSE
           m=m+1
           values( m+IJ)=RVIND
        END IF
        m=m+1
        values( m+IJ)=rvind                                 !  007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR   M                 
        IF(KDEC(42).NE.MINDIC) THEN
           CALL IC2700(KDEC(42),ICOVER)
           if(ICOVER.ne. 999999) then
              m=m+1
              values( m+IJ)=ICOVER                             !  020010  CLOUD COVER (TOTAL)                       %                 
           else
             m=m+1
             values( m+IJ)=RVIND
           end if
        ELSE
           ICOVER=999999
           m=m+1
           values( m+IJ)=RVIND
        END IF
        ILT=999
        if(KDEC(45).NE.MINDIC.AND.KDEC(45).NE.14) THEN
           ILT=KDEC(45)+30
        END IF
        IMT=999
        if(KDEC(46).NE.MINDIC.AND.KDEC(46).NE.14) THEN
           IMT=KDEC(46)+20
        END IF
        IHT=999
        if(KDEC(47).NE.MINDIC.AND.KDEC(47).NE.14) THEN
           IHT=KDEC(47)+10
        END IF

        m=m+1
        IF(ILT.EQ.999.AND.IMT.EQ.999.AND.IHT.EQ.999) THEN
           values( m+IJ)=RVIND                                ! 008002 vertical significance
        ELSEIF(ILT.NE.999.AND.ILT.NE.30) THEN
           values( m+IJ)=7.
        ELSEIF(IMT.NE.999.AND.IMT.NE.20) THEN
           values( m+IJ)=8.
        ELSE
           values( m+IJ)=9.
        END IF
        IF(ICOVER.EQ.113) THEN
           values( m+IJ)=5.
        ELSEIF(ICOVER.EQ.0) THEN
           values( m+IJ)=62.
        ELSEIF(ICOVER.EQ.999999) THEN
           values( m+IJ)=RVIND
        END IF
C
        m=m+1
        IF(ICOVER.EQ.0) THEN
           values( m+IJ)=0.
        ELSEIF(ICOVER.EQ.113) THEN
           values( m+IJ)=9
        ELSEIF(KDEC(44).NE.MINDIC) THEN
           values( m+IJ)=KDEC(44)                        ! 020011 cloud amount
        ELSE
           values( m+IJ)=RVIND
        END IF
C
        m=m+1
        if(ist.eq.669) then
           print*,'icover=',icover,KDEC(77),KDEC(43)
        end if
        IF(ICOVER.EQ.0) THEN
           values( m+IJ)=RVIND
        ELSEIF(ICOVER.EQ.999999) THEN
           IF(KDEC(77).NE.MINDIC) THEN
              values( m+IJ)=KDEC(77)
           END IF
        ELSEIF(ICOVER.EQ.113) THEN
           IF(KDEC(77).NE.MINDIC) THEN
              values( m+IJ)=KDEC(77)
           ELSE
              values( m+IJ)=RVIND
           END IF
        ELSE
           IF(KDEC(43).NE.MINDIC.and.KDEC(43).NE.16381.and.
     1        KDEC(43).NE.16382.and.KDEC(43).NE.14) THEN
              values( m+IJ)=KDEC(43)
              IF(KDEC(77).NE.MINDIC) THEN
                 values( m+IJ)=KDEC(77)                    !  020013 height of base of cloud
              END IF
           ELSE
              values( m+IJ)=RVIND
           END IF
        END IF
C
        m=m+1
        IF(KINT(18).EQ.0) then
           values( m+IJ)=30.
        ELSEIF(KINT(18).EQ.9) THEN
           values( m+IJ)=62.
        ELSEIF(kint(18).EQ.MINDIC) THEN
           values( m+IJ)=RVIND
        ELSEIF(KDEC(45).NE.MINDIC.AND.KDEC(45).NE.14) THEN
           values( m+IJ)=KDEC(45)+30.                    !  020012  CLOUD TYPE                                CODE TABLE 020012 
        ELSE
           values( m+IJ)=rvind
        END IF
        m=m+1
        IF(KINT(18).EQ.0) then
           values( m+IJ)=20.
        ELSEIF(kint(18).EQ.MINDIC) THEN
           values( m+IJ)=RVIND
        ELSEIF(KINT(18).EQ.9.OR.KDEC(46).EQ.MINDIC.OR.
     1        KDEC(46).EQ.14) THEN
           values( m+IJ)=61.
        ELSEIF(KDEC(46).NE.MINDIC.AND.KDEC(46).NE.14) THEN
           values( m+IJ)=KDEC(46)+20.                    !  020012  CLOUD TYPE                                CODE TABLE 020012
        ELSE
           values( m+IJ)=rvind
        END IF
        m=m+1
        IF(KINT(18).EQ.0) then
           values( m+IJ)=10.
        ELSEIF(kint(18).EQ.MINDIC) THEN
           values( m+IJ)=RVIND
        ELSEIF(KINT(18).EQ.9.OR.KDEC(47).EQ.MINDIC.OR.
     1         KDEC(47).EQ.14) THEN
           values( m+IJ)=60.
        ELSEIF(KDEC(47).NE.MINDIC.AND.KDEC(47).NE.14) THEN
           values( m+IJ)=KDEC(47)+10.                    !  020012  CLOUD TYPE                                CODE TABLE 020012
        ELSE
           values( m+IJ)=rvind
        END IF

        m=m+1
        values( m+IJ)=4.                                 !  031001  DELAYED DESCRIPTOR REPLICATION FACTOR     NUMERIC           
C
        m=m+1
        IF(KINT(15).LE.3) THEN                           ! ix 
           IF(ICOVER.EQ.113) THEN
              values( m+IJ)=5                            ! 008002 vertical significance
           ELSEIF(ICOVER.EQ.999999) THEN
              values( m+IJ)=RVIND
           ELSE
              IF(KDEC(75).NE.MINDIC) THEN
                 values( m+IJ)=1.
              else
                 values( m+IJ)=rvind
              end if
           END IF
        ELSEIF(KINT(15).GT.3.AND.KINT(15).LE.7) THEN
           IF(KDEC(75).EQ.9) THEN
              values( m+IJ)=5
           ELSE
              IF(KDEC(75).NE.MINDIC) THEN
                 values( m+IJ)=21
              else
                 values( m+IJ)=rvind
              end if
           END IF
        ELSE
           values( m+IJ)=RVIND
        END IF
C
        m=m+1
        IF(KDEC(75).NE.MINDIC) THEN
           values( m+IJ)=KDEC(75)                        !  020011  CLOUD AMOUNT                              CODE TABLE 020011 
        ELSE
           values( m+IJ)=RVIND
        END IF

        m=m+1
        IF(KINT(18).EQ.9 ) then
          if(KDEC(77).NE.MINDIC) THEN
             values( m+IJ)=59.
          else
             values( m+IJ)=rvind
          end if
        ELSEIF(KINT(18).EQ.MINDIC) THEN
           values( m+IJ)=RVIND
        ELSEIF(KDEC(76).NE.MINDIC) THEN
           values( m+IJ)=KDEC(76)                      ! 020012  cloud type
        ELSE
           values( m+IJ)=RVIND
        END IF
C
        m=m+1
        IF(KDEC(77).NE.MINDIC) THEN
           values( m+IJ)=KDEC(77)                        !  020013  HEIGHT OF BASE OF CLOUD                   M                 
        ELSE
           values( m+IJ)=RVIND
        END IF
        m=m+1
C-----

C------
        values( m+IJ)=2.                                 !  008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATI  CODE TABLE 008002 
        IF(KINT(15).GT.3.AND.KINT(15).LE.7) values( m+IJ)=22.
        IF(KDEC(78).EQ.MINDIC) THEN
           values( m+IJ)=rvind
        end if
        m=m+1
        IF(KDEC(78).NE.MINDIC) THEN
           values( m+IJ)=KDEC(78)                        !  020011  CLOUD AMOUNT                              CODE TABLE 020011 
        ELSE
           values( m+IJ)=RVIND
        END IF

        m=m+1
        IF(KINT(18).EQ.9) THEN
           if(KDEC(80).NE.MINDIC) THEN
              values( m+IJ)=59.
           else
              values( m+IJ)=rvind
           end if
        ELSEIF(KINT(18).EQ.MINDIC) THEN
           values( m+IJ)=RVIND
        ELSEIF(KDEC(79).NE.MINDIC) THEN
           values( m+IJ)=KDEC(79)                      ! 020012  cloud type
        ELSE
           values( m+IJ)=RVIND
        END IF

        m=m+1
        IF(KDEC(80).NE.MINDIC) THEN
           values( m+IJ)=KDEC(80)                        !  020013  HEIGHT OF BASE OF CLOUD                   M                 
        ELSE
           values( m+IJ)=RVIND
        END IF
        m=m+1
        values( m+IJ)=3.                                 !  008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATI  CODE TABLE 008002 
        IF(KINT(15).GT.3.AND.KINT(15).LE.7) values( m+IJ)=23.
        IF(KDEC(81).EQ.MINDIC) THEN
           values( m+IJ)=rvind
        end if
        m=m+1
        IF(KDEC(81).NE.MINDIC) THEN
           values( m+IJ)=KDEC(81)                        !  020011  CLOUD AMOUNT                              CODE TABLE 020011 
        ELSE
           values( m+IJ)=RVIND
        END IF

        m=m+1
        IF(KINT(18).EQ.9) THEN
           if(KDEC(83).NE.MINDIC) THEN
              values( m+IJ)=59.
           else
              values( m+IJ)=rvind
           end if
        ELSEIF(KINT(18).EQ.MINDIC) THEN
           values( m+IJ)=RVIND
        ELSEIF(KDEC(82).NE.MINDIC) THEN
           values( m+IJ)=KDEC(82)                      ! 020012  cloud type
        ELSE
           values( m+IJ)=RVIND
        END IF


        m=m+1
        IF(KDEC(83).NE.MINDIC) THEN
           values( m+IJ)=KDEC(83)                        !  020013  HEIGHT OF BASE OF CLOUD                   M                 
        ELSE
           values( m+IJ)=RVIND
        END IF
        m=m+1
        values( m+IJ)=4.                                 !  008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATI  CODE TABLE 008002
        IF(KINT(15).GT.3.AND.KINT(15).LE.7) values( m+IJ)=24.
        IF(KDEC(84).EQ.MINDIC) THEN
           values( m+IJ)=rvind
        end if
        m=m+1
        IF(KDEC(84).NE.MINDIC) THEN
           values( m+IJ)=KDEC(84)                        !  020011  CLOUD AMOUNT                              CODE TABLE 020011
        ELSE
           values( m+IJ)=RVIND
        END IF

        m=m+1
        IF(KINT(18).EQ.9) THEN
           if(KDEC(86).NE.MINDIC) THEN
              values( m+IJ)=59.
           else
              values( m+IJ)=rvind
           end if
        ELSEIF(KINT(18).EQ.MINDIC) THEN
           values( m+IJ)=RVIND
        ELSEIF(KDEC(85).NE.MINDIC) THEN
           values( m+IJ)=KDEC(85)                      ! 020012  cloud type
        ELSE
           values( m+IJ)=RVIND
        END IF

        m=m+1
        IF(KDEC(86).NE.MINDIC) THEN
           values( m+IJ)=KDEC(86)                        !  020013  HEIGHT OF BASE OF CLOUD                   M
        ELSE
           values( m+IJ)=RVIND
        END IF


        IF(KINT(149).NE.MINDIC) THEN
           m=m+1
           values( m+IJ)=1.          !  031001  DELAYED DESCRIPTOR REPLICATION FACTOR     NUMERIC           
           m=m+1
           values( m+IJ)=RVIND       !  vertical sign
           m=m+1
           values( m+IJ)=RVIND       !  020011  CLOUD AMOUNT                              CODE TABLE 020011 
           IF(ICOVER.EQ.113.OR.ICOVER.EQ.999999) THEN
              values( m+IJ)=RVIND
           ELSE
             IF(KINT(150).NE.MINDIC) THEN
               values( m+IJ)=KINT(150) !  cloud amount
               values( m-1+IJ)=11.         !  008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATI  CODE TABLE 008002 
               IF(ICOVER.EQ.113.OR.ICOVER.EQ.999999) THEN
                  values( m-1+IJ)=10.
               END IF
             END IF
           END IF
           
              IF(KINT(151).NE.MINDIC) THEN
                 m=m+1
                IF(ICOVER.EQ.113.OR.ICOVER.EQ.999999) THEN
                   values( m+IJ)=RVIND
                ELSE
                 values( m+IJ)=KINT(151)   !  020012  CLOUD TYPE                                CODE TABLE 020012
                END IF
              ELSE
                 m=m+1
                 values( m+IJ)=RVIND       
              END IF
              m=m+1
              values( m+IJ)=RVIND       !  020014  HEIGHT OF TOP OF CLOUD                    M                 
              IF(ICOVER.EQ.113.OR.ICOVER.EQ.999999) THEN
                 values( m+IJ)=RVIND
              ELSE
                IF(KINT(152).NE.MINDIC) THEN
                   values( m+IJ)=KINT(152)*100.
                   IF(values( m+IJ).GT.values( 12+IJ)) THEN
                      values( m-3+IJ)=10.
                   END IF
                END IF
              END IF
              m=m+1
              values( m+IJ)=RVIND       !  020017  CLOUD TOP DESCRIPTION                     CODE TABLE 020017 
              IF(ICOVER.EQ.113.OR.ICOVER.EQ.999999) THEN
                 values( m+IJ)=RVIND
              ELSE
                IF(KINT(153).NE.MINDIC) THEN
                   values( m+IJ)=KINT(153)
                END IF
              END IF
        ELSE
           m=m+1
           values( m+IJ)=0.          !  031001  DELAYED DESCRIPTOR REPLICATION FACTOR     NUMERIC
        END IF
c
        m=m+1
        values( m+IJ)=7.          !  008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATI  CODE TABLE 008002 
        m=m+1
        values( m+IJ)=RVIND       !  020054  (VAL) TRUE DIRECTION FROM WHICH CLOUDS A  DEGREE TRUE       
        m=m+1
        values( m+IJ)=8.          !  008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATI  CODE TABLE 008002 
        m=m+1
        values( m+IJ)=RVIND       !  020054  (VAL) TRUE DIRECTION FROM WHICH CLOUDS A  DEGREE TRUE       
        m=m+1
        values( m+IJ)=9.          !  008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATI  CODE TABLE 008002 
        m=m+1
        values( m+IJ)=RVIND       !  020054  (VAL) TRUE DIRECTION FROM WHICH CLOUDS A  DEGREE TRUE       
        m=m+1
        values( m+IJ)=RVIND       !  008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATI  CODE TABLE 008002 
        m=m+1
        values( m+IJ)=RVIND       !  005021  BEARING OR AZIMUTH                        DEGREE TRUE       
        m=m+1
        values( m+IJ)=RVIND       !  007021  ELEVATION (SEE NOTE 2)                    DEGREE            
        m=m+1
        values( m+IJ)=RVIND       !  020012  CLOUD TYPE                                CODE TABLE 020012 
        m=m+1
        values( m+IJ)=RVIND       !  005021  BEARING OR AZIMUTH                        DEGREE TRUE       
        m=m+1
        values( m+IJ)=RVIND       !  007021  ELEVATION (SEE NOTE 2)                    DEGREE            
        m=m+1
        IF(KINT(94).NE.MINDIC) THEN
           values( m+IJ)=KINT(94)     ! E
        elseif(KINT(99).NE.MINDIC) THEN
           values( m+IJ)=KINT(99)    !  E'
        else
           values( m+IJ)=RVIND       !  020062  STATE OF THE GROUND (WITH OR WITHOUT SNO  CODE TABLE 020062 
        end if
        IF(KDEC(99).NE.MINDIC) THEN
            m=m+1
           values( m+IJ)=KDEC(99)/10.       !  013013  TOTAL SNOW DEPTH                          M                 
        ELSEIF(KDEC(97).NE.MINDIC.AND.KDEC(2).EQ.6) THEN
           m=m+1
           values( m+IJ)=0.0
        ELSE
           m=m+1
           values( m+IJ)=RVIND
        END IF
        m=m+1
        IF(KDEC(70).NE.MINDIC) THEN
           values( m+IJ)=KDEC(70)+273.15
        ELSE
           values( m+IJ)=RVIND       !  012113  GROUND MINIMUM TEMPERATURE, PAST 12 HOUR  K                 
        END IF
        IF(KDEC(28).NE.MINDIC) THEN
           m=m+1
           values( m+IJ)=KDEC(28)       !  020003  PRESENT WEATHER (SEE NOTE 1)              CODE TABLE 020003 
           IF(KDEC(4).EQ.14) values( m+IJ)=KDEC(28)+100.
        ELSE
           m=m+1
           values( m+IJ)=RVIND
        END IF
        IH=NINT(values(8+IJ))
        IF(IH.eq.0.or.IH.eq.6.or.IH.eq.12.or.IH.eq.18) then
           m=m+1
           values( m+IJ)=-6.     ! 004024 TIME PERIOD OR DISPLACEMENT
        ELSEIF(IH.eq.3.or.IH.eq.9.or.IH.eq.15.or.IH.eq.21) then
           m=m+1
           values( m+IJ)=-3.     ! 004024 TIME PERIOD OR DISPLACEMENT
        ELSE
           m=m+1
           values( m+IJ)=-1.     ! 004024 TIME PERIOD OR DISPLACEMENT
        END IF
        MHPAST=nint(values( m+IJ))
        IF(KDEC(29).NE.MINDIC) THEN
           m=m+1
           values( m+IJ)=KDEC(29)           !  020004  PAST WEATHER (1) (SEE NOTE 2)             CODE TABLE 020004
           IF(KDEC(4).EQ.14) values( m+IJ)=KDEC(29)+10.
        ELSE
           m=m+1
           values( m+IJ)=RVIND
        END IF
        IF(KDEC(30).NE.MINDIC) THEN
           m=m+1
           values( m+IJ)=KDEC(30)           !  020005  PAST WEATHER (2) (SEE NOTE 2)             CODE TABLE 020005
           IF(KDEC(4).EQ.14) values( m+IJ)=KDEC(30)+10.
        ELSE
           m=m+1
           values( m+IJ)=RVIND
        END IF
        m=m+1
        values( m+IJ)=-1.         !  004024  TIME PERIOD OR DISPLACEMENT               HOUR              
        m=m+1
        values( m+IJ)=RVIND       !  014031  TOTAL SUNSHINE                            MINUTE            
        m=m+1
        values( m+IJ)=-24.        !  004024  TIME PERIOD OR DISPLACEMENT               HOUR              
        if(KDEC(111).ne.MINDIC) then
           m=m+1
           values( m+IJ)=KDEC(111)   !  014031  TOTAL SUNSHINE                            MINUTE
        else
           m=m+1
           values( m+IJ)=RVIND       
        end if
        m=m+1
        if(RH_prec(idx).ne.99.9) then
        values( m+IJ)=RH_prec(idx)       !  007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR   M                 
        else
        values( m+IJ)=rvind
        end if
        IF(KDEC(41).NE.MINDIC) THEN
           m=m+1
           values( m+IJ)=-KDEC(41)     !  004024  TIME PERIOD OR DISPLACEMENT               HOUR
        ELSE
           m=m+1
           values( m+IJ)=RVIND
        END IF
        IF(KDEC(40).NE.MINDIC) THEN
           m=m+1
           IF(KDEC(40).EQ.0) THEN
              values( m+IJ)=0.               !  013011  TOTAL PRECIPITATION/TOTAL WATER EQUIVALE  KG/M**2 TRACE
           ELSE
              values( m+IJ)=KDEC(40)/10.       !  013011  TOTAL PRECIPITATION/TOTAL WATER EQUIVALE  KG/M**2           
           END IF
        ELSE
           m=m+1
           values( m+IJ)=RVIND
        END IF
        IF(KDEC(141).NE.MINDIC) THEN
           m=m+1
           IF(KINT(115).ne.MINDIC) THEN
             values( m+IJ)=-KDEC(141)            !  004024  TIME PERIOD OR DISPLACEMENT               HOUR
           else
             values( m+IJ)=-3.
           end if
        ELSE
           m=m+1
           values( m+IJ)=RVIND
        END IF
        IF(KDEC(140).NE.MINDIC) THEN
           m=m+1
           IF(KDEC(140).EQ.0) THEN
              values( m+IJ)=0          !  013011  TOTAL PRECIPITATION/TOTAL WATER EQUIVALE  KG/M**2.
           ELSE
              values( m+IJ)=KDEC(140)/10.  !  013011  TOTAL PRECIPITATION/TOTAL WATER EQUIVALE  KG/M**2
           END IF
        ELSE
           m=m+1
           values( m+IJ)=RVIND
        END IF
        m=m+1
        if(RH_tem(idx).ne.99.9) then 
        values( m+IJ)=RH_tem(idx)       !  007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR   M                 
        else
        values( m+IJ)=rvind
        end if
        if(KDEC(72).ne.MINDIC) then
           m=m+1
           values( m+IJ)=-KDEC(72)        !  004024  TIME PERIOD OR DISPLACEMENT               HOUR              
        else
           m=m+1
           values( m+IJ)=RVIND
        end if
        m=m+1
        if(KDEC(72).ne.MINDIC) then
           values( m+IJ)=0.          !  004024  TIME PERIOD OR DISPLACEMENT               HOUR              
        else
           values( m+IJ)=RVIND
        end if
        if(KDEC(71).NE.MINDIC) THEN
           m=m+1
           values( m+IJ)=KDEC(71)/10.+273.15       !  012111  MAXIMUM TEMPERATURE, AT HEIGHT AND OVER   K                 
        else
           m=m+1
           values( m+IJ)=RVIND
        end if
        if(KDEC(74).NE.MINDIC) then
           m=m+1
           values( m+IJ)=-KDEC(74)        !  004024  TIME PERIOD OR DISPLACEMENT               HOUR              
        else
           m=m+1
           values( m+IJ)=rvind
        end if
        m=m+1
        if(KDEC(74).NE.MINDIC) then
           values( m+IJ)=0.          !  004024  TIME PERIOD OR DISPLACEMENT               HOUR              
        else
           values( m+IJ)=RVIND
        END IF
        if(KDEC(73).ne.mindic) then
           m=m+1
           values(m+IJ)=KDEC(73)/10.+273.15       !  012112  MINIMUM TEMPERATURE, AT HEIGHT AND OVER   K                 
        else
           m=m+1
           values(m+IJ)=rvind
        end if
        m=m+1
        if(RH_wind(idx).ne.99.9) then
        values(m+IJ)=RH_wind(idx)                         !  007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR   M                 
        else
        values(m+IJ)=rvind
        end if
        m=m+1
        if(KINT(3).le.1) values(m+IJ)=8.
        if(KINT(3).eq.3.or.KINT(3).eq.4) values(m+IJ)=12.   !  002002  TYPE OF INSTRUMENTATION FOR WIND MEASURE  FLAG TABLE 002002 
        m=m+1
        values(m+IJ)=2.          !  008021  TIME SIGNIFICANCE                         CODE TABLE 008021 
        m=m+1
        values(m+IJ)=-10.                        !  004025  TIME PERIOD OR DISPLACEMENT               MINUTE            
        IF(KDEC(25).NE.MINDIC) THEN
           m=m+1
           values(m+IJ)=KDEC(25)                    !  011001  WIND DIRECTION                            DEGREE TRUE       
        ELSE
           m=m+1
           values(m+IJ)=RVIND
        END IF
        IF(KDEC(26).NE.MINDIC) THEN
           m=m+1
           values(m+IJ)=KDEC(26)                    !  011002  WIND SPEED                                M/S               
        ELSE
           m=m+1
           values(m+IJ)=RVIND
        END IF
        m=m+1
        values(m+IJ)=RVIND       !  008021  TIME SIGNIFICANCE                         CODE TABLE 008021 
        m=m+1

        iidir=0
        iiten=0
        i11=0
        IPERIOD=0
        do ii=138,147,3
        if(kint(ii).eq.7) then
C          period
           CALL IC4077(kint(ii+1),MINDIC,IPERIOD)
        elseif(kint(ii).eq.10) then
C          highest gust -10 minutes
           iiten=ii
        elseif(kint(ii).eq.11) then
C          highest gust -period
           i11=ii
        elseif(kint(ii).eq.15) then
C          highest gust wind direction
           iidir=ii
        end if
        end do

        values(m+IJ)=-10.        !  004025  TIME PERIOD OR DISPLACEMENT               MINUTE

        m=m+1
        if(iidir.ne.0.and.iiten.ne.0) then
         if(kint(iidir).eq.15.and.kint(iidir+1).ne.mindic.and.
     1      kint(iiten).eq.10) then
         values(m+IJ)=iidir       !  011043  MAXIMUM WIND GUST DIRECTION               DEGREE TRUE
         else
         values(m+IJ)=RVIND       !  
         end if
        else
         values(m+IJ)=RVIND
        end if
        m=m+1
        if(iiten.ne.0) then
          if(kint(iiten).eq.10.and.kint(iiten+1).ne.mindic) then
          if(kint(3).eq.1) then
          values(m+IJ)=kint(iiten+1)   !  011041  MAXIMUM WIND GUST SPEED     M/S
          else
            values(m+IJ)=kint(iiten+1)*.5148  !011041  MAXIMUM WIND GUST SPEED   KNOTS
          end if
        else
          values(m+IJ)=RVIND      
        end if
        else
          values(m+IJ)=RVIND
        end if
        m=m+1
        IF(IPERIOD.EQ.0) THEN
        values(m+IJ)=MHPAST*60.       !  004025  TIME PERIOD OR DISPLACEMENT               MINUTE            
        ELSE
           values(m+IJ)=IPERIOD
        END IF
        m=m+1
        if(iidir.ne.0.and.i11.ne.0) then
          if(kint(iidir).eq.15.and.kint(iidir+1).ne.mindic.and.
     1          kint(i11).eq.11) then
          values(m+IJ)=kint(iidir)   !  011043  MAXIMUM WIND GUST DIRECTION               DEGREE TRUE
          else
            values(m+IJ)=RVIND       
          end if
        else
          values(m+IJ)=RVIND
        end if
        m=m+1
        if(i11.ne.0) then
          if(kint(i11).eq.11.and.kint(i11+1).ne.mindic) then
          if(kint(3).eq.1) then
          values(m+IJ)=kint(i11+1)   !  011041  MAXIMUM WIND GUST SPEED                   M/S
          else
            values(m+IJ)=kint(i11+1)*.5148 !011041  MAXIMUM WIND GUST SPEED   KNOTS
          end if
          else
          values(m+IJ)=RVIND       
          end if
        else
          values(m+IJ)=RVIND
        end if
        m=m+1
        values(m+IJ)=RVIND       !  007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR   M                 
        m=m+1
        values(m+IJ)=-24.        !  004024  TIME PERIOD OR DISPLACEMENT               HOUR              
        m=m+1
        values(m+IJ)=RVIND       !  002004  TYPE OF INSTRUMENTATION FOR EVAPORATION   CODE TABLE 002004 
        if(kdec(110).ne.MINDIC) then
           m=m+1
           values(m+IJ)=KDEC(110)       !  013033  EVAPORATION/EVAPOTRANSPIRATION            KG/M**2           
        else
           m=m+1
           values(m+IJ)=RVIND
        end if
        m=m+1
        values(m+IJ)=-1.         !  004024  TIME PERIOD OR DISPLACEMENT               HOUR              
        m=m+1
        values(m+IJ)=RVIND       !  014002  LONG-WAVE RADIATION, INTEGRATED OVER PER  J/M**2            
        m=m+1
        values(m+IJ)=RVIND       !  014004  SHORT-WAVE RADIATION, INTEGRATED OVER PE  J/M**2            
        m=m+1
        values(m+IJ)=RVIND       !  014016  NET RADIATION, INTEGRATED OVER PERIOD SP  J/M**2            
        m=m+1
        values(m+IJ)=RVIND       !  014028  GLOBAL SOLAR RADIATION (HIGH ACCURACY),   J/M**2            
        m=m+1
        values(m+IJ)=RVIND       !  014029  DIFFUSE SOLAR RADIATION (HIGH ACCURACY),  J/M**2            
        m=m+1
        values(m+IJ)=RVIND       !  014030  DIRECT SOLAR RADIATION (HIGH ACCURACY),   J/M**2            
        m=m+1
        values(m+IJ)=-24.        !  004024  TIME PERIOD OR DISPLACEMENT               HOUR              
        m=m+1
        values(m+IJ)=RVIND       !  014002  LONG-WAVE RADIATION, INTEGRATED OVER PER  J/M**2            
        m=m+1
        values(m+IJ)=RVIND       !  014004  SHORT-WAVE RADIATION, INTEGRATED OVER PE  J/M**2            
        if(kdec(112).ne.mindic) then
           m=m+1
           values(m+IJ)=KDEC(112)*1000       !  014016  NET RADIATION, INTEGRATED OVER PERIOD SP  J/M**2            
        else
           m=m+1
           values(m+IJ)=RVIND
        end if
        m=m+1
        values(m+IJ)=RVIND       !  014028  GLOBAL SOLAR RADIATION (HIGH ACCURACY),   J/M**2            
        m=m+1
        values(m+IJ)=RVIND       !  014029  DIFFUSE SOLAR RADIATION (HIGH ACCURACY),  J/M**2            
        m=m+1
        values(m+IJ)=RVIND       !  014030  DIRECT SOLAR RADIATION (HIGH ACCURACY),   J/M**2            
        m=m+1
        values(m+IJ)=RVIND       !  004024  TIME PERIOD OR DISPLACEMENT               HOUR              
        m=m+1
        values(m+IJ)=RVIND       !  004024  TIME PERIOD OR DISPLACEMENT               HOUR              
        m=m+1
        values(m+IJ)=RVIND       !  012049  (VAL) TEMPERATURE CHANGE OVER SPECIFIED   K                 
C
C
c       DO i=1,20
c       CVALS(1)(I:I)=char(255)
c       END DO
C
       END IF
      ELSE

        if(NSUB .ne.0) then
C
C       Fill in KDATA
C
        IF(NSUB .GT. KDLEN) THEN
           print*,'Number of subsets greater than KDLEN parameter'
           call exit(2)
        END IF
        iix=0
        do iii=1,NSUB
        ij=(iii-1)*kelem
        iix=iix+1
        KDATA(iix)=4
        iix=iix+1
        KDATA(iix)=nint(values(54+IJ))
        end do 
C
        KTDLEN=1
        KTDLST(1)=307080

C       Find first subset which is not NIL
     
        inil=0
        do i=1,NSUB
        ij=(i-1)*kelem
        if(abs(values(5+ij)-rvind)/rvind .gt.eps) then
           inil=i
           exit
        end if
        end do

        IF(INIL .eq.0) Then
           print*,'NIL bulletin detected'
           return
        END IF
C
C       Fill in KSEC0
C
        KSEC0(1)=0
        KSEC0(2)=0
        KSEC0(3)=4
C
C       Fill in KSEC1
C
        KSEC1( 1)=22        ! length
        KSEC1( 2)=4         ! bufr edition
        KSEC1( 3)=NCENTRE        ! originating centre
        KSEC1( 4)=KDEC(21)         ! update sequence number
        KSEC1( 5)=0       ! presence of section 2
        KSEC1( 6)=0         ! bufr message type (bufr table A)
        KSEC1( 7)=170       ! bufr message subtype
        KSEC1( 8)=0
        
        KSEC1( 9)=NINT(values(5+(INIL-1)*kelem)) ! year
        IF(KSEC1( 2).le.3) then
           KSEC1( 9)=mod(nint(values(5+(INIL-1)*kelem)),100)
           if(KSEC1( 9).eq.0) KSEC1( 9)=100
        END IF
        KSEC1(10)=NINT(values(6+(INIL-1)*kelem))
        KSEC1(11)=NINT(values(7+(INIL-1)*kelem))
        KSEC1(12)=NINT(values(8+(INIL-1)*kelem))
        KSEC1(13)=NINT(values(9)+(INIL-1)*kelem)
        KSEC1(14)=0         ! bufr master tables used
        KSEC1(15)=14        ! version of master table used
        KSEC1(16)=0         ! originating sub-centre
        KSEC1(17)=0         ! international sub-category
        if(KSEC1(12).EQ.3.or.KSEC1(12).EQ.9.or.
     1   KSEC1(12).EQ.15.or.KSEC1(12).EQ.21) KSEC1(17)=1
        if(KSEC1(12).EQ.0.or.KSEC1(12).EQ.6.or.
     1   KSEC1(12).EQ.12.or.KSEC1(12).EQ.18) KSEC1(17)=2
        KSEC1(18)=0         ! second
C
C     Fill in KSEC2

        NYEAR  =NINT(VALUES(5+(inil-1)*kelem))
        NMONTH =NINT(VALUES(6+(inil-1)*kelem))
        NDAY   =NINT(VALUES(7+(inil-1)*kelem))
        NHOUR  =NINT(VALUES(8+(inil-1)*kelem))
        NMINUTE=NINT(VALUES(9+(inil-1)*kelem))
        NSECOND=0
C
        NLAT1=NINT(VALUES(10+(inil-1)*kelem)*100000)+9000000
        NLON1=NINT(VALUES(11+(inil-1)*kelem)*100000)+18000000
        NLAT2=0
        NLON2=0
        CIDENT= CHAR(KINT(09))//CHAR(KINT(10))//CHAR(KINT(11))//
     1        CHAR(KINT(12))//CHAR(KINT(13))//'    '  

        NTYPE=1           ! SURFACE DATA
        NSBTYPE=170       ! ! SYNOP LAND
        IF(KSEC1(5).eq.128) then
         key( 1)=52
         key( 2)=NTYPE
         key( 3)=NSBTYPE
         key( 4)=NYEAR
         key( 5)=NMONTH
         key( 6)=NDAY
         key( 7)=NHOUR
         key( 8)=NMINUTE
         key( 9)=NSECOND
         key(10)=NLON1
         key(11)=NLAT1
         key(12)=NLON1
         key(13)=NLAT1
         key(14)=1
         key(15)=0
         WRITE(CIDENT(1:2),'(I2.2)') NINT(VALUES(1+IJ))
         WRITE(CIDENT(3:5),'(I3.3)') NINT(VALUES(2+IJ))
         key(16)=ichar(cident(1:1))
         key(17)=ichar(cident(2:2))
         key(18)=ichar(cident(3:3))
         key(19)=ichar(cident(4:4))
         key(20)=ichar(cident(5:5))
         key(21)=32
         key(22)=32
         key(23)=32
         key(24)=32
C
         KSEC2(1)=52
      
         NOBS=1
         NRECR=1
         NOBS=NSUB
         NBUFTYPE=0
C    
         NCORR=0
         NNIL=0
C
         NQC=70
C     
         CALL ASCTIM(CTIME)
         READ(CTIME,'(I2,10X,I2,1X,I2,1X,I2,1X,I2)') IDD,IHH,IMM,ISS,ICC
C
         key(26)=idd
         key(27)=ihh
         key(28)=imm
         key(29)=iss
         key(30)=NRDAY
         key(31)=NRHOUR
         key(32)=NRMIN
         key(33)=NRSEC
c
         do jjj=34,45
         key(jjj)=0
         end do
c
         key(46)=70
         kerr=0
         call bupkey(key,ksec1,ksec2,kerr)
         if(kerr.ne.0) then
           print*,'BUPKEY: error',kerr
           call exit(2)
         end if
        END IF

C       Fill in KSEC3
C
        KSEC3(1)=0
        KSEC3(2)=0
        KSEC3(3)=NSUB     ! number of subsets
        KSEC3(4)=128    ! uncompressed observation
C
C
        KPMISS=1
        KPRUS=1
        NOKEY=0
        CALL BUPRQ(KPMISS,KPRUS,NOKEY)

C       Pack BUFR
C
        KBUFL=JBUFL
        CALL BUFREN(KSEC0 ,KSEC1,KSEC2,KSEC3,KSEC4,
     1            KTDLEN,KTDLST,KDLEN,KDATA,
     2            KELEM,KVALS,VALUES,CVALS,KBUFL,KBUFR,IERR)
        IF(IERR.GT.0) THEN
          RETURN
        END IF

C
C       WRITE DATA INTO OUTPUT FILE
C
        KBUFL=KBUFL*4
        CALL PBWRITE(IUNIT1,KBUFR,KBUFL,IERR)
        if(ierr.ge.0) then
           ierr=0
        end if
C
        end if
      END IF
C
C     -----------------------------------------------------------------
      RETURN
      END
      SUBROUTINE KTOMPSI(IA)
C
C
C**** *KTOMPSI*
C
C
C     PURPOSE.
C     --------
C
C         CONVERTS KNOTS TO METRES PER SECOND, ROUNDING
C         TO NEAREST METRE.
C
C         INPUT    :  IA WIND SPEED IN KNOTS (INTEGER)
C
C         OUTPUT   :  IA WIND SPEED IN M/S (INTEGER)
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *KTOMPSI(IA)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
C
C     ------------------------------------------------------------------
C
C*          1.   CONVERT WIND IN KNOTS TO METER PER SECOND .
C                -------------------------------------------
 100  CONTINUE
C
C
      IA=INT(0.5148 * IA +0.5)
      RETURN
      END
      FUNCTION P(Z)
C
      DATA A/5.252368255329/, B/44330.769230769/
      DATA  C/0.000157583169442/
      DATA  PTRO/226.547172/, PO/1013.25/
C
      IF (Z.GT.11000.) GO TO 50
      Y = 1.-Z/B
      P = PO*(Y**A)
      RETURN
C
50    Y = -C*(Z-11000.)
      P = PTRO*EXP(Y)
      RETURN
      END
      SUBROUTINE SAVBULL ( IERR )
C
C
C**** *SAVBULL*
C
C
C     PURPOSE.
C     --------
C
C         WRITE COMPLETE BULLETIN TO ERROR FILE .
C
C         BULLETIN IN KCHAR(1)-KCHAR(IGS) IN CCITT 5.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SAVBULL(IERR)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
      INCLUDE 'comindx.h'
      INCLUDE 'combuff.h'
C     character*256 cf
C
C
      CHARACTER*80 YLINE,YLINEA
      CHARACTER*1 Y63
      CHARACTER*4 YGS
      CHARACTER*3 YCRLF
      CHARACTER*15000 YBUFF
C
      Y63=CHAR(63)
      YGS=CHAR(13)//CHAR(13)//CHAR(10)//CHAR(3)
      YCRLF=CHAR(13)//CHAR(13)//CHAR(10)
C     ------------------------------------------------------------------
C*          1.   OPEN ERROR FILE AT THE BEGINNING OF THE PROCESS.
C                -------------------------------------------------
 100  CONTINUE
C
C
      YBUFF=' '
      YLINE=' '
      YLINEA=' '
      I1=1
C
      IP = 1
      JP = IP
      J = IGS
C
C*          1.1  OUTPUT INITIAL CONTROL CHARACTERS.
C                ----------------------------------
 110  CONTINUE
C
      CALL NEXTPRT (JP,IGS)
      K = JP - IP
      N1 = 0
      DO 111 N=1,K
         N1 = N1 + 1
         IF ( KCHAR(IP).GT.127) THEN
                                   YLINEA(N1:N1)=Y63
                                   ISIGN=1
                                   KCHAR(IP)=IAND(KCHAR(IP),127)
                                          END IF
         YLINE(N1:N1) = CHAR(KCHAR(IP))
         IP = IP + 1
C
  111 CONTINUE
C
      YBUFF(I1:)=YLINE(1:N1)
      I1=I1+N1
      IF(ISIGN.EQ.1) THEN
         YBUFF(I1:)=YLINEA(1:N1)
         I1=I1+N1
         YBUFF(I1:)=YCRLF
         I1=I1+3
         ISIGN=0
      END IF
C
      YLINE=' '
      YLINEA=' '
C
 120  CONTINUE
C
      CALL NEXTPRT (IP,J)
      IF (IP .GT. J) GO TO 400
      JP = IP
      CALL NEXTEND (JP,J)
      CALL NEXTPRT (JP,J)
      K = JP - IP
      N1 = 0
      DO 112 N=1,K
         N1 = N1 + 1
         IF ( KCHAR(IP).GT.127 )
     C                      THEN
                               YLINEA(N1:N1) = Y63
                               ISIGN= 1
                               KCHAR(IP) = IAND(KCHAR(IP),127)
                            END IF
C
      YLINE(N1:N1) = CHAR(KCHAR(IP))
      IP = IP + 1
C
  112 CONTINUE
C
      YBUFF(I1:)=YLINE(1:N1)
      I1=I1+N1
      IF(ISIGN.EQ.1) THEN
         YBUFF(I1:)=YLINEA(1:N1)
         I1=I1+N1
         YBUFF(I1:)=YCRLF
         I1=I1+3
         ISIGN=0
      END IF
C
      YLINE=' '
      YLINEA=' '
C
      GO TO 120
C
 400  CONTINUE
C
      YBUFF(I1:)=YGS(1:4)
      I1=I1+4
C
C     REMOVE PARITY BIT
C
      DO 410 I = 1 , IGS
      KCHAR(I) = IAND(KCHAR(I) , 127)
 410  CONTINUE
C
C     WRITE BULLETIN IN ERROR IN EMPRESS ERROR DB.
C
C     CALL PUT_ERROR_BULL('SYNO',I1,YBUFF,IERR)
C 
      RETURN
      END
      SUBROUTINE SAVREP ( IHEAD,IERR)
C
C
C**** *SAVREP*
C
C
C     PURPOSE.
C     --------
C
C         WRITE REPORT IN ERROR TO THE ERROR FILE.
C
C                 IHEAD = 0 , WRITE BULLETIN HEADER AND ERROR REPORT
C                             TO ERROR FILE.
C                       = 1 , WRITE ERROR REPORT ONLY.
C                       = 2 , WRITE 'GS' CHARACTER AT END.
C
C                 IERR NOT USED.
C
C                 IHEAD SET TO 1 IF HEADER WRITTEN , OTHERWISE
C                       UNCHANGED.
C
C                 IERR SET TO -1 , IF ERROR IN WRITE , OTHERWISE
C                       UNCHANGED.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SAVREP(IHEAD,IERR)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
      INCLUDE 'comindx.h'
      INCLUDE 'combuff.h'
      INCLUDE 'comerror.h'
      INCLUDE 'comerrorc.h'
      character* 256 youtfile
      CHARACTER*15000 YBUFF
C     character*256 cf
C
      DIMENSION ISTART(4),IFIN(4)
C
      CHARACTER *80 YLINE,YLINEA
      CHARACTER*1 Y63
      CHARACTER*4 YGS
      CHARACTER*3 YCRLF
C
      Save YBUFF
C
      Y63=CHAR(63)
      YGS=CHAR(13)//CHAR(13)//CHAR(10)//CHAR(3)
      YCRLF=CHAR(13)//CHAR(13)//CHAR(10)
C     ------------------------------------------------------------------
C*          1.   OPEN ERROR FILE AT THE BEGINNING OF THE PROCESS.
C                -------------------------------------------------
 100  CONTINUE
C
      YLINE=' '
      YLINEA=' '
      ISIGN=0
C
C
C*          1.2  WRITE BULLETIN HEADER TO ERROR
C                ------------------------------
C                FILE IF NOT ALREADY DONE.
C                -------------------------
 120  CONTINUE
C
      IF ( IHEAD.NE.0 ) GO TO 130
C
      IF(IHEAD.EQ.0) THEN
         I1=1
         YBUFF=' '
      END IF
C
      ISTART(1) = 1
      ISTART(2) = ISL
      ISTART(3) = IAH
      ISTART(4) = IMI
C
      IFIN(1) = ISL
      IFIN(2) = IAH
      IFIN(3) = JAH
      CALL NEXTEND(IFIN(3),IGS)
      CALL NEXTPRT(IFIN(3),IGS)
      IFIN(4) = JMI
      CALL NEXTEND(IFIN(4),IGS)
      CALL NEXTPRT(IFIN(4),IGS)
C
      IP = 0
      N2 = 0
C
      IHEAD = 1
C
      NN = 4
C
      DO 121 N=1,NN
         K = IFIN(N) - ISTART(N) 
         IF(K.GT.80) K=80
         DO 122 N1=1,K
            N2 = N2 + 1
            IP = IP + 1
            IF (KCHAR(IP).GT.127)
     C                      THEN
                                YLINEA(N2:N2) = Y63
                                ISIGN=1
                                KCHAR(IP)=IAND(KCHAR(IP),127)
                            END IF
C
            YLINE(N2:N2) = CHAR(KCHAR(IP))
C
  122    CONTINUE
C
C
C
      YBUFF(I1:)=YLINE(1:N2)
      I1=I1+N2
      IF(ISIGN.EQ.1) THEN
         YBUFF(I1:)=YLINEA(1:N2)
         I1=I1+N2
         YBUFF(I1:)=YCRLF
         I1=I1+3
         ISIGN=0
      END IF
C
         YLINE=' '
         YLINEA=' '
C
      N2 = 0
C
  121 CONTINUE
C
C
C*           1.3   WRITE ERROR REPORT , IF REQUIRED.
C***               ---------------------------------
 130  CONTINUE
C
      IF ( IHEAD.EQ.2 ) GO TO 140
C
C
C     AVOID WRITING REPORT TWICE IF MIMIMJMJ  LINE IS
C     MISSING.
C
      IF ( IAH.EQ.KPT ) THEN
           RETURN
      END IF
C
C     AVOID WRITING MIMIMJMJ TWICE ( CORRUPT MIMIMJMJ CAN BE TAKEN
C     AS STATION NUMBER )
C
      IF ( KPT.LT.IFIN(4) ) KPT = IFIN(4)
C
C
      IP = KPT
      IF(KDEC(4) .EQ. 35 .OR. KDEC(4) .EQ. 36) IP = IMI
      IF(KDEC(4) .EQ. 32 .OR. KDEC(4) .EQ. 33) IP = IMI
      J = IEQ+ 3
      IF ( J.GT.IGS) J = IGS
C
  133 CALL NEXTPRT (IP,J)
      IF (IP.GT.J) THEN
          RETURN
       END IF
      JP = IP
      CALL NEXTEND(JP,J)
      CALL NEXTPRT(JP,J)
      K = JP - IP
      N1 = 0
      IF(K.GT.80) K=80
      DO 131 N=1,K
         N1 = N1 + 1
         IF ( KCHAR(IP).GT.127)
     C                        THEN
                                  YLINEA(N1:N1) = Y63
                                  ISIGN= 1
                                  KCHAR(IP) = IAND(KCHAR(IP),127)
                              END IF
C
         YLINE(N1:N1) = CHAR(KCHAR(IP))
         IP = IP + 1
C
 131  CONTINUE
C
      YBUFF(I1:)=YLINE(1:N1)
      I1=I1+N1
      IF(ISIGN.EQ.1) THEN
         YBUFF(I1:)=YLINEA(1:N1)
         I1=I1+N1
         YBUFF(I1:)=YCRLF
         I1=I1+3
         ISIGN=0
      END IF
C
      YLINE=' '
      YLINEA=' '
C
C
      GO TO 133
C
C
C*              1.4 WRITE 'GS' AT END OF BULLETIN.
C                   ------------------------------
 140  CONTINUE
C
      YLINE(1:4)= YGS
C
      YBUFF(I1:)=YLINE(1:4)
      I1=I1+4
C
C     WRITE BULLETIN INTO DB
C
C     if(Nmode.eq.0) then
C        CALL PUT_ERROR_BULL('SYNO',I1,YBUFF,IERR)
C     else
C        open(11,file=youtfile,err=200,
C    1           form='formatted',
C    2           status='unknown')
C        write(11,err=210,iostat=ios,fmt='(a)') ybuff(1:i1)
C        close(11)
C        ierr=1
C     end if
C
      GO TO 400
C
 400  CONTINUE
C
C     REMOVE PARITY BIT
C
      DO 500 I = 1 , IGS
      KCHAR(I) = IAND(KCHAR(I) , 127)
 500  CONTINUE
C
C
      RETURN
C
 200  continue
c
      print*,'Open error on ',youtfile
      call exit(2)
c
 210  continue
c
      print*,'write error on ',youtfile
      call exit(2)
      return
      END
      SUBROUTINE ERRSTA (IPART,IMARK,IFIRST,NUMBER)
C
C
C
C**** *ERRSTA*
C
C
C     PURPOSE.
C     --------
C
C         COUNTS THE NUMBER OF ERRORS IN THE REPORT
C         COUNTS THE NUMBER OF ERRORS IN THE DECODING JOB
C         MARKS THE ERROR BIT TO KDEC
C         ADDS ?-MARK TO KCHAR AT THE ERRONEUS GROUP
C
C         INPUT     : IPART    - INDICATOR OF NOER
C                                (IN TEMPS  3 = A, 4 = B, 5 = C, 6 = D)
C                     IMARK    = 1 IF ? IS TO BE ADDED TO ERRONEUS GROUP
C                     IFIRST   = O IF FIRST DECODING ATTEMPT OF THE REPORT
C                              = 1 OTHERWISE
C                     NUMBER   - NUMBER OF ERRORS IN THE REPORT SO FAR
C
C         OUTPUT    : NUMBER   - NEW NUMBER OF ERRORS
C                     NOER     - NUMBER OF ERRONEUS REPORT (INCREASED BY 1
C                                IF THE FIRST ERROR IN THE REPORT)
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *ERRSTA(IPART,IMARK,IFIRST,NUMBER)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         *XXXX* *XXXXXXX(XXXX)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          A.HOLOPAINEN  NOV.83
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
      INCLUDE 'comindx.h'
      INCLUDE 'comstat.h'
C
C     ------------------------------------------------------------------
C
C*          1.                                     .
C                -----------------------------------
 100  CONTINUE
C
      IF(NUMBER .GE. 0) NUMBER = NUMBER + 1
      IF ( IFIRST.EQ.0 )
     C   THEN
            IF(NUMBER .EQ. 1)
     C          NUMRERR(IPART)=NUMRERR(IPART) + 1
            NOER(IPART,KERR)=NOER(IPART,KERR) + 1
C            KERBIT =ISHFT(KDEC(20),1-KERR) .AND. 1
C            IF(KERBIT .EQ. 0) KDEC(20) = KDEC(20) + 2**(KERR-1)
         END IF
C
      IPT = IABS(IPT)
      IF(IMARK .EQ. 1) KCHAR(IPT) = IOR(KCHAR(IPT) , 128)
      IF(IMARK .EQ. 2)
     C   THEN
            NPT = IPT
            CALL NEXSEP2(NPT,IEQ,*200)
            CALL PREPRT(NPT,IMI,*200)
            KCHAR(NPT) = IOR(KCHAR(NPT) , 128)
         END IF
C
200   CONTINUE
C
      RETURN
      END
      SUBROUTINE REMEEE
C
C
C**** *REMEEE*
C
C
C     PURPOSE.
C     --------
C
C         HANDLE TYPING ERRORS CORRECTED BY THE 'E E E'
C         PROCEDURE AS SPECIFIED IN GTS MANUAL.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *REMEEE*
C
C         INPUT     : REPORT IN KCHAR(IPT) - KCHAR(IEQ) , IN CCITT 5 ,
C                     1 CHARACTER PER WORD.
C
C         OUTPUT    : E'S , ERRONEUS CHARACTERS AND REPEATED GROUPS REPLACED
C                     BY SPACE CHARACTERS. THESE ARE IGNORED IN SCANNING
C                     ROUTINES.
C
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
      INCLUDE 'comindx.h'
C
C     ------------------------------------------------------------------
C*          1.  CHECK FOR 'E E E'. 'EEE' IS ACCEPTED EVEN
C               THOUGH THIS MAY BE  AMBIGUOUS WITH A SYNOP '333' GROUP IN
C               LETTER SHIFT. FREQUENTLY  ONLY 1 OR 2 'E'S MAY BE USED.
C               THESE ALSO CATERED FOR.
 100  CONTINUE
C
C     SKIP PAST SHIP'S CALL SIGN AND LOCATE 'E' IF ANY EXISTS.
C
      I = IPT + 4
      N = 69
      CALL NEXTVAL(I,N,IEQ)
      IF(I .GT. IEQ) RETURN
C
C     'E' CHARACTER FOUND. REPLACE 'E' AND ANY FOLLOWING 'E'S BY SPACES
C     E.G.  40118 7012EE 40118 70500  BECOMES
C           40118 7012   40118 70500 .
C
      K = I
      CALL NEXTFIG(K,IEQ)
      IF(K .GE. IEQ) RETURN
C
            DO 101 J=I,K-1
               IF((KCHAR(J) .NE. 10) .AND. (KCHAR(J) .NE. 13))
     1             KCHAR(J) = 32
101         CONTINUE
C
C     SET POINTER TO CHARACTER BEFORE THE 'E'. CHANGE
C     THIS CHARACTER TO A 'SPACE'.
C       E.G.  40118 7012   40118 70500  BECOMES
C             40118 701    40118 70500 .
C
      N = I - 1
      IF((KCHAR(N) .NE. 10) .AND. (KCHAR(N) .NE. 13))
     1    KCHAR(N) = 32
C
C     SCANNING BACKWARDS REPLACE CHARACTERS BY 'SPACE'
C     UNTIL A 'SPACE' CHARACTER IS ENCOUNTERED.
C        E.G.  40118 701    40118 70500  BECOMES
C              40118        40118 70500 .
C
      DO 102 I=N-1,IPT,-1
          IF(KCHAR(I) .EQ. 32) GO TO 300
          IF((KCHAR(I) .NE. 10) .AND. (KCHAR(I) .NE. 13))
     1        KCHAR(I) = 32
  102 CONTINUE
C
C     NO CHARACTER FOUND
C
      GO TO 100
C
C     ---------------------------------------------------------------------
C*           3. 'I' NOW POINTS TO THE 'SPACE' AFTER LAST FIGURE OF THE GROUP
C               BEFORE THE SERIES OF SPACES AND 'K' POINTS TO FIRST FIGURE
C               OF FOLLOWING GROUP. IF THESE GROUPS ARE THE SAME REMOVE
C               ONE GROUP (SECOND).
C               E.G.  40118        40118 70500  BECOMES
C                     40118              70500 .
C
 300  CONTINUE
C
      I = I - 5
      IF((KCHAR(I)   .EQ. KCHAR(K))   .AND.
     1   (KCHAR(I+1) .EQ. KCHAR(K+1)) .AND.
     2   (KCHAR(I+2) .EQ. KCHAR(K+2)) .AND.
     3   (KCHAR(I+3) .EQ. KCHAR(K+3)) .AND.
     4   (KCHAR(I+4) .EQ. KCHAR(K+4)))
     5         THEN
                   N = K + 4
                   DO 301 I=K,N
                       KCHAR(I) = 32
  301              CONTINUE
               END IF
C
C
C*           4.  SOMETIMES MORE THAN 1 GROUP HAS TO BE DELETED
C                E.G.  40118 59623 7012EE 40118 70500 .
C                THIS WILL NOW HAVE BECOME
C                40118 59623        40118         AND POINTERS ARE
C                      I            K
C                40118 59623 NEED TO BE REMOVED.
 400  CONTINUE
C
      I = I - 6
      N = K - 1
      IF((KCHAR(I)   .EQ. KCHAR(K))   .AND.
     1   (KCHAR(I+1) .EQ. KCHAR(K+1)) .AND.
     2   (KCHAR(I+2) .EQ. KCHAR(K+2)) .AND.
     3   (KCHAR(I+3) .EQ. KCHAR(K+3)) .AND.
     4   (KCHAR(I+4) .EQ. KCHAR(K+4)))
     5         THEN
                   DO 401 J=I,N
                       KCHAR(J) = 32
  401              CONTINUE
               END IF
C
C     GO BACK TO BEGINNING OF SUBROUTINE TO FIND OUT
C     IF THERE ARE MORE 'E'-CORRECTIONS.
C
      GO TO 100
C
      END
      SUBROUTINE NEXTMI(I,J,II)
C
C
C
C**** *NEXTMI*
C
C
C     PURPOSE.
C     --------
C
C         TO FIND NEXT MIMIMJMJ GROUP IN THE BULLETIN.
C         SCANS BULLETIN IN 'KCHAR' FOR NEXT GROUP OF
C         ('TTAA' OR 'TTBB' OR 'TTCC' OR 'TTDD' ETC.)
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTMI(I,J,II)*
C
C         INPUT     : I - SCAN STARTS AT WORD I.
C                     J - SCAN STOPS AT WORD J .
C
C
C         OUTPUT    : II- POSITION OF THE FIRST CHARACTER
C                         IN REQUIRED GROUP
C                         IF CHARACTER NOT FOUND II = 99999
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.h'
      INCLUDE 'comwork.h'
C
      DIMENSION MIMJ(26)
C
C
      DATA MIMJ /65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,
     &           80,81,82,83,84,85,86,87,88,89,90/
C     ------------------------------------------------------------------
C*          1.   FIND NEXT MIMIMJMJ GROUP.
C                -------------------------
 100  CONTINUE
C
C
      II=9999
      K =IABS(I)
      M =IABS(I)
C
 101  CONTINUE
C
      CALL NEXSEP2(M,J,*1000)
      CALL NEXPRT2(M,J,*1000)
C
      IF(M.GE.J) RETURN
C
      K1= KCHAR(M  )
      K2= KCHAR(M+1)
      K3= KCHAR(M+2)
      K4= KCHAR(M+3)
C
      DO 102 N=1,26
C
      IF(K1.EQ.MIMJ(N).AND.K2.EQ.MIMJ(N))
     &   THEN
C
            DO 103 NN=1,26
C
            IF(K3.EQ.MIMJ(NN).AND.K4.EQ.MIMJ(NN))
     &         THEN
                  CALL PRESEP(M,K,*1000)
                  CALL PREPRT(M,K,*1000)
                  II= M+1
                  RETURN
               END IF
C
 103        CONTINUE
C
         END IF
C
 102  CONTINUE
C
       GO TO 101
C
 1000 CONTINUE
C
      RETURN
      END
      subroutine Read_GTS(IUNIT,YOUT,K,IERR)
C
C
C     This subroutine returns ascii time in form
C
C     dd-mmm-yyyy hh:mm:ss.cc
C
      character*8 cdate
      character*10 ctime
      character*5 zone
      integer     itimes(8)
      character*8 yppdate
      character*3 ymonth(12)
      character*11 cdatepp
C
      data ymonth/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     1            'Aug','Sep','Oct','Nov','Dec'/
C
      CHARACTER*23 CDATETIME
C
C------------------------------------------------------------------------------------
      yppdate=' '
      call getenv('PP_DATE',yppdate)
C
      if(yppdate.eq.' ') then
         CALL DATE_AND_TIME(cdate,ctime,zone,itimes)
       
      
         CDATETIME=cdate(7:8)//'-'//ymonth(itimes(2))//'-'//
     1             cdate(1:4)//' '//ctime(1:2)//':'//ctime(3:4)//
     2             ':'//ctime(5:9)

C
      else
         CALL DATE_AND_TIME(cdate,ctime,zone,itimes)
         cdatepp(3:3)='-'
         cdatepp(7:7)='-'
c
c        get pp date
c
         read(yppdate(5:6),'(i2.2)') imm

         cdatepp(8:11)=yppdate(1:4)
         cdatepp(4:6)=ymonth(imm)
         cdatepp(1:2)=yppdate(7:8)
C
         CDATETIME=CDATEPP(1:11)//' '//ctime(1:2)//':'//ctime(3:4)//
     1             ':'//ctime(5:9)
      end if
C
      RETURN
      END
      integer function iymd2c(idate)
c
c**** *iymd2c*
c
c
c     purpose.
c     --------
c         returns century day for given yyyymmdd.
c
c**   interface.
c     ----------
c
c         *iymd2c(idate)*
c
c          input :  idate (yyyymmdd)
c
c          output:  idate (century day)
c
c
c     method.
c     -------
c
c          none.
c
c
c     externals.
c     ----------
c
c         none.
c
c     reference.
c     ----------
c
c          none.
c
c     author.
c     -------
c
c          m.  dragosavac    *ecmwf*       21/02/98.
c
c
c     modifications.
c     --------------
c
c          none.
c
c
      implicit logical(o,g), character*8(c,h,y)
c
      dimension idm(13)
c
      data idm/0,31,28,31,30,31,30,31,31,30,31,30,31/  
c
c     ------------------------------------------------------------------
c*          1.   set month.
c                ----------
 100  continue
c
      idays=0
c
      iym=idate/100 
      id=idate-iym*100
      iy=iym/100
      im=iym-iy*100
      iyear=iy
c
      if(iyear.gt.2000) then
         do 111 i=2001,iyear-1
          idays=idays+365
          if(mod(i,4).eq.0) idays=idays+1
 111     continue
c
         do 112 i=1,im
          idays=idays+idm(i)
          if(i.eq.3) then
             if(mod(iyear,4).eq.0) idays=idays+1
          end if
 112     continue
c
         idays=idays+id
c
         iymd2c=idays
      else
         do 101 i=1900,iyear-1
          idays=idays+365
          if(mod(i,4).eq.0) idays=idays+1
 101     continue
c
         do 102 i=1,im
          idays=idays+idm(i) 
          if(i.eq.3) then
             if(mod(iyear,4).eq.0) idays=idays+1
          end if
 102     continue
c
         idays=idays+id-1
c
         iymd2c=idays
      end if
c
      return
      end 
      integer function ic2ymd(kday)
c
c**** *ic2ymd*
c
c
c     purpose.
c     --------
c         returns integer yyyymmdd for given century day.
c
c**   interface.
c     ----------
c
c         *i=ic2ymd(kday)*
c
c          input :  kday
c
c          output:  i (in form of yyyymmdd)
c
c
c     method.
c     -------
c
c          none.
c
c
c     externals.
c     ----------
c
c         none.
c
c     reference.
c     ----------
c
c          none.
c
c     author.
c     -------
c
c          m.  dragosavac    *ecmwf*       21/02/98.
c
c
c     modifications.
c     --------------
c
c          none.
c
c
      implicit logical(o,g), character*8(c,h,y)
c
      dimension idm(12)
c
      data idm/31,28,31,30,31,30,31,31,30,31,30,31/  
c
c     ------------------------------------------------------------------
c*          1.   set year.
c                ---------
 100  continue
c
c     If number of days zero or negative it is previous century
c
      if(kday.le.0) then
         kday=kday+36891
      end if
c
      if(kday.gt.36891) then
         kday=kday-36891
      end if
c
c     first 10 years of 20 century will not be used.
c     This number of years will be interpreted as 21st century
c
      iy=1900
      if(kday.lt.7300) iy=2001
c
      do 101 i= 1,100
      if(mod(i,4).eq.0) then
         iday=kday-366
         if(iday.gt.0) then
            iy=iy+1
            kday=iday
         else
            go to 200
         end if 
      else
         iday=kday-365
         if(iday.gt.0) then
            iy=iy+1
            kday=iday
         else
            go to 200
         end if
      end if
 101  continue
c
c           2. set month.
c              ----------
 200  continue
c
      if(mod(iy,4).eq.0.and.iy.le.2000) kday=kday+1

      do 201 i=1,12
c
c     print*,'kday=',kday
      iday=kday-idm(i)
      if(i.eq.2) then
         if(mod(iy,4).eq.0) iday=kday-29
      end if
c
      if(iday.le.0) then
         im=i
         id=kday
         go to 300
      else
         kday=iday
      end if
 201  continue
c
c*          3. set yyyymmd.
c              ----------
 300  continue
c
      ic2ymd=iy*10000+im*100+id
c
      return
      end 
      integer function iymdhm2m(ktime)
c
c**** *iymdhm2m*
c
c
c     purpose.
c     --------
c         calculate time in minutes since 1/1/1978,
c         given input as ktime(1)  year (1992)
c                        ktime(2)  month
c                        ktime(3)  day
c                        ktime(4)  hour
c                        ktime(5)  minute
c
c**   interface.
c     ----------
c
c         *iymdhm2m(ktime)*
c
c          input :  ktime(5)
c
c
c     method.
c     -------
c
c          none.
c
c
c     externals.
c     ----------
c
c         none.
c
c     reference.
c     ----------
c
c          none.
c
c     author.
c     -------
c
c          m. dragosavac    *ecmwf*       21/10/89.
c
c
c     modifications.
c     --------------
c
c          none.
c
c
      implicit logical(l,o,g), character*8(c,h,y)
c
      integer ktime(5)
c
      dimension idm(13)
c
      data idm/0,31,28,31,30,31,30,31,31,30,31,30,31/  
c
c     ------------------------------------------------------------------
c*          1.  set minutes.
c               ------------
100   continue
c
      idays=0
c
      do 101 i=1978,ktime(1)-1
       idays=idays+365
       if(mod(i,4).eq.0) idays=idays+1
 101  continue
c
      do 102 i=1,ktime(2)
       idays=idays+idm(i) 
       if(i.eq.3) then
          iy=ktime(1)
          if(mod(iy,4).eq.0) idays=idays+1
       end if
 102  continue
c
      idays=idays+ktime(3)-1
c
      itm=idays*1440+ktime(4)*60+ktime(5)
c
      iymdhm2m=itm
c
      return
      end 
      integer function ictime2m(ctime)
c
c**** *ictime2m*
c
c
c     purpose.
c     --------
c         calculate time in minutes since 1/1/1978,
c         given input ctime as ascii time on vms
c
c**   interface.
c     ----------
c
c         *ictime2m*
c
c
c     method.
c     -------
c
c          none.
c
c
c     externals.
c     ----------
c
c         none.
c
c     reference.
c     ----------
c
c          none.
c
c     author.
c     -------
c
c          m. dragosavac    *ecmwf*       21/10/89.
c
c
c     modifications.
c     --------------
c
c          none.
c
c
      implicit logical(l,o,g), character*8(c,h,y)
c
      character*23 ctime
      character*3  ymonth(12)
c
      dimension idm(13)
c
      data idm/0,31,28,31,30,31,30,31,31,30,31,30,31/  
      data ymonth/'jan','feb','mar','apr','may','jun','jul',
     1            'aug','sep','oct','nov','dec'/
c
c     ------------------------------------------------------------------
c*          1.  set minutes.
c               ------------
100   continue
c
      idays=0
c
      do 101 i=1,12
      if(ctime(4:6).eq.ymonth(i)) then
         im=i
         im=im+1
         go to 110
      end if
 101  continue
c
 110  continue
c
      read(ctime,'(i2,5x,i4,1x,i2,1x,i2)') id,iy,ih,imin
c
      do 111 i=1978,iy-1
       idays=idays+365
       if(mod(i,4).eq.0) idays=idays+1
 111  continue
c
      do 112 i=1,im-1
       idays=idays+idm(i) 
       if(i.eq.3) then
          if(mod(iy,4).eq.0) idays=idays+1
       end if
 112  continue
c
      idays=idays+id-1
c
      itm=idays*1440+ih*60+imin
c
      ictime2m=itm
c
      return
      end 
      subroutine daymn(ydate,n)
C
c
c**** *daypn*
c
c
c     purpose.
c     --------
c         calculate date from ydate plus n days.
c
c
c**   interface.
c     ----------
c
c         none.
c
c     method.
c     -------
c
c          none.
c
c
c     externals.
c     ----------
c
c          none.
c
c     reference.
c     ----------
c
c          none.
c
c     author.
c     -------
c          m. dragosavac    *ecmwf*       15/02/98.
c
c
c     modifications.
c     --------------
c
c          none.
c
c
      implicit logical(l,o,g), character*8(c,h,y)
c
c
      character*8  ydate
c
      read(ydate(1:8),'(i8)') idate
      idays=iymd2c(idate)
      idays=idays-n
      idate=ic2ymd(idays)
      write(ydate(1:8),'(i8.8)') idate
c
c
      return
      end
      subroutine daypn(ydate,n)
C
C
c
c**** *daypn*
c
c
c     purpose.
c     --------
c         calculate date from ydate plus n days.
c
c
c**   interface.
c     ----------
c
c         none.
c
c     method.
c     -------
c
c          none.
c
c
c     externals.
c     ----------
c
c          none.
c
c     reference.
c     ----------
c
c          none.
c
c     author.
c     -------
c
c          m. dragosavac    *ecmwf*       15/02/87.
c
c
c     modifications.
c     --------------
c
c          none.
c
c
      implicit logical(o,g), character*8(c,h,y)
c
c
      character*8  ydate
c
      read(ydate(1:8),'(i8)') idate
      idays=iymd2c(idate)
      idays=idays+n
      idate=ic2ymd(idays)
      write(ydate(1:8),'(i8.8)') idate
c
c
      return
      end
      subroutine juldate(kjday,kyear,kmonth,kday)
C
C
C
C**** *datum*
C
C
C     PURPOSE.
C     --------
C         DEFINE PROPER MONTH AND YEAR IF DAY IS DEFINED.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *DATUM(IDD,IMM,IYY,kerr)*
C
C                       IDD - DAY
C                       IMM - MONTH
C                       IYY - YEAR
C
C     METHOD.
C     -------
C
C          IF IDD IS GREATER THAN CURRENT DAY DAY IS CONSIDERED TO BE FROM
C          PREVIOUS MONTH.IF CURRENT MONTH IS JANUARY
C          YEAR BECOMS PREVIOUS ONE.
C          IF IDD IS LESS OR EQUALL THAN CURRENT DAY IT IS FROM CURRENT MONTH
C          AND YEAR.
C
C
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *DATE(YDATE)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      CHARACTER*3 YMONTH(12)
      character*23 ydtime
C
      DATA YMONTH/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     1            'Aug','Sep','Oct','Nov','Dec'/
C
CC
C     ------------------------------------------------------------------
C
C*          1.   GET DATE FROM THE SYSTEM.
C                -------------------------
 100  CONTINUE
CC
      CALL asctim(ydtime)
      READ(ydtime( 8:11),'(I4.4)') IYEAR
      READ(ydtime(1:2),'(I2.2)') IDAY
C
      DO 101 I=1,12
       IF(ydtime(4:6).EQ.YMONTH(I)) IMONTH=I
 101  CONTINUE
CC
C
C*          1.1  DEFINE MONTH AND YEAR
C                ---------------------
 110  CONTINUE
C
      IF(IDAY.GE.IDD) THEN
         IMM=IMONTH
         IYY=IYEAR
         RETURN
      END IF
C
      IF(IDAY.LT.IDD) THEN
         IF(IMONTH.EQ.1) THEN
            IMM=12
            IYY=IYEAR-1
            RETURN
         END IF
         IMM=IMONTH-1
         IYY=IYEAR
      END IF
C
      RETURN
      END
      subroutine next_date(cdate,ctime,cdelta,cdate1,ctime1)
C
C
c
c**** *next_date*
c
c
c     purpose.
c     --------
c
c
c
c**   interface.
c     ----------
c
c         none.
c
c     method.
c     -------
c
c          none.
c
c
c     externals.
c     ----------
c
c          none.
c
c     reference.
c     ----------
c
c          none.
c
c     author.
c     -------
c
c          m. dragosavac    *ecmwf*       15/02/99.
c
c
c     modifications.
c     --------------
c
c          none.
c
c
      implicit logical(o,g), character*8(c,h,y)
c
c
      character*8  cdate,cdate1
      character*4  ctime,ctime1,cdelta
c
      read(ctime(1:2),'(i2)') ihour
      read(ctime(3:4),'(i2)') imin
      read(cdelta,'(i4)') idelta
c
      imin1=ihour*60+imin+idelta
c
      ihour2=imin1/60
      imin2=imin1-ihour2*60
c
      if(ihour2.gt.24) then
         ihour2=ihour2-24
         write(ctime1(1:2),'(i2.2)') ihour2
         write(ctime1(3:4),'(i2.2)') imin2
c
         cdate1=cdate
         call daypn(cdate1,1)
c
      else
         cdate1=cdate
         write(ctime1(1:2),'(i2.2)') ihour2
         write(ctime1(3:4),'(i2.2)') imin2
      end if
c
      return
      end
      subroutine daypdelta(ky,km,kd,kdelta,ky1,km1,kd1)
C
c
      idate=ky*10000+km*100+kd
c
      icentury_day=iymd2c(idate)
c
      icentury_day=icentury_day+kdelta
c
      new_date=ic2ymd(icentury_day)
c
      ky1=new_date/10000
      idiff= (new_date-ky1*10000)
      km1=idiff/100
      kd1=idiff-km1*100
c
      return
      end
      SUBROUTINE STATION_TEXT(IERR)

C**** *STATION_TEXT*
C
C
C     PURPOSE.
C     --------
C         READ IN STATION LIST 
C         ( WMO VOLUMEN A - LIST OF OBSERVING STATIONS)
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *STATION_TEXT(IERR)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 2009.

      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'comstation.h'
      include 'combase.h'
      character*256 cf
C
      character*2 csp00,csp03,csp06,csp09,csp12,csp15,csp18,csp21
      character*1 cuat00,cuat06,cuat12,cuat18
      character*32 cstation

C
C     ------------------------------------------------------------------
C*          1.   READ IN STATION LIST.
C                ---------------------
 100  CONTINUE
C
      i=index(cppbase,' ')
      i=i-1

      cf=' '
      cf=cppbase(1:i)//'/dat/synop2bufr_station.txt'
      i=index(cf,' ')
      i=i-1
c
      OPEN(UNIT=4,IOSTAT=IOS,ERR=300,
     1     FILE=cf(1:i),
     1     STATUS='OLD',
     1     FORM='formatted')
C
      NST=0
 200  continue
      
      NST=NST+1
      read(4,fmt=8889,iostat=ios,END=400)  irgcoun(nst), istid(nst),
     1                           rlatid(nst), rlongid(nst),
     2                           isthp(nst), istha(nst) ,ipcode(nst),
     3                           csp00(nst),csp03(nst),csp06(nst),
     4                           csp09(nst),csp12(nst),csp15(nst),
     5                           csp18(nst),csp21(nst),
     6                           cuat00(nst),cuat06(nst),cuat12(nst),
     7                           cuat18(nst),cstation(nst),RH_tem(nst),
     8                           RH_vis(nst),RH_prec(nst),RH_wind(nst)
c
 8889 format(i4,1x,i5.5,1x,f7.2,1x,f7.2,1x,i4,1x,i4,1x,i1,
     1       1x,8(a2,1x),4(a,1x),a,1x,F4.1,3(3x,F4.1))
      if(ios.ne.0) then
         print*,'Read error on ',cf(1:i)
         call exit(2)
      end if

      go to 200
c

 400  continue
C
      NST=NST-1
      CLOSE(4)
C
      return
 300  continue
      print*,'open error on ',cf(1:i)

      END
      SUBROUTINE IC4077(ICODE,MINDIC,IPERIOD)
C
C
C****
C*
C*    NAME     : IC4077
C*
C*    FUNCTION :  DECODE TIME PERIOD
C*
C*    INPUT    :  ICODE   - CODE for preriod
C*                MINDIC  - MISSING VALUE
C*
C*    OUTPUT   :  IPERIOD - in minutes
C*
C*
C****
C
C
C***   SET MISSING VALUE
C
      IPERIOD=MINDIC
C
C***
      if(icode.eq.mindic) return

      if(icode.ge.0.and.icode.lt.69) then
       if(icode.eq.0) then
         iperiod=0
       elseif(icode.eq.69) then
         iperiod=MINDIC
       elseif(icode.ge.61.and.icode.lt.68) then
         iperiod=-((icode/10)*60+30)
       elseif(icode.eq.67) then
         iperiod=-15*60
       elseif(icode.eq.68) then
         iperiod=-18*60
       else
         ihour=icode/10
         iminutes=icode-ihour*10
         iperiod=-(ihour*60+iminutes*6)
       end if
      end if
C
      RETURN
      END

