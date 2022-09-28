C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE CCF1CR (HEXP,HSTR,HDIS,HTYP,HREP,HLEV,HARE,HPER,HFIR,
     C      HOWN,HBAK,HCLS,HFAM,HRUT,KPR,KRET)
C
C
C---->
C**** CCF1CR
C
C     PURPOSE.
C     --------
C
C           CREATE REQUESTED NODES IN CFS FOR MARS ARCHIVING.
C
C**   INTERFACE.
C     ----------
C
C           CALL CCF1CR (HEXP,HSTR,HDIS,HTYP,HREP,HLEV,HARE,HPER,HFIR,
C    C      HOWN,HBAK,HCLS,HFAM,HRUT,KPR,KRET)
C
C           INTEGER    K.
C           REAL       P.
C           LOGICAL    O.
C           CHARACTER  H.
C
C               INPUT PARAMETERS.
C               -----------------
C
C               HEXP       - 4 CHARACTER EXPERIMENT IDENTIFIER.
C
C               HSTR       - ARCHIVE STREAM.
C                            FOR RESEARCH.
C                               DA - DAILY ARCHIVE.
C                            FOR OPERATIONS.
C                               DA - DAILY ARCHIVE.
C                               CH - CHERNOBYL ARCHIVE.
C                               TG - TOGA ARCHIVE.
C                               MO - MONTHLY.
C
C               HDIS       - NEW , NEW ENTRY REQUIRED.
C                            OLD , DELETE OLD ENTRY BEFORE MAKING
C                                  NEW ONE.
C
C               HTYP       - AN , ANALYSIS.
C                            IA , INITIALISED ANALYSIS.
C                            FG , FIRST GUESS.
C                            FC , FORECAST.
C                            EA , ERRORS IN ANALYSIS.
C                            CL , CLIMATOLOGY.
C
C
C               HREP       - SH , SPHERICAL HARMONICS.
C                            GG , GAUSSIAN GRID.
C                            LL , LAT/LONG GRID.
C
C               HLEV       - M , MODEL LEVELS.
C                            P , PRESSURE LEVELS.
C                            S , SURFACE.
C
C               HARE       - G , GLOBAL DATA.
C
C               HPER       - S , SHORT RETENTION PERIOD.
C                            M , MEDIUM RETENTION PERIOD.
C                            P , PERMANENT RETENTION.
C
C               HFIR       - DATE OF FIRST ARCHIVE (YYMMDD).
C
C               HOWN       - OWNER'S PASSWORD. 6 CHARACTERS FOR
C                            RESEARCH EXPERIMENTS, 5 FOR OPERATIONS.
C
C               HBAK       - 0 , NO BACKUP NODES REQUIRED.
C                            1 , 1 BACKUP OF NODES REQUIRED.
C
C               HCLS       - RD , RESEARCH DEPARTMENT.
C                            OD , OPERATIONS DEPARTMENT.
C
C               HFAM       - N, NO FAMILY ATTRIBUTE IN NODES.
C                            Y , FAMILY ATTRIBUTES IN NODES.
C
C               HRUT       - ROOT NODE TO BE USED.
C                            FOR RESEARCH
C                               P , PRODUCTION ROOT MARSR.
C                               T , TEST ROOT MATSR.
C                            FOR OPERATIONS
C                               P , PRODUCTION ROOT MARST.
C                               O , OLD PRODUCTION ROOT MARSO.
C                               T , TEST ROOT MATST.
C
C               KPR        - DEBUG PRINT SWITCH.
C                            0 , NO PRINTOUT.
C                            1 , DEBUG PRINTOUT.
C
C               OUTPUT PARAMETERS.
C               ------------------
C
C               KRET       - ERROR RETURN CODE.
C                            0   , NO ERROR ENCOUNTERED.
C                            1   , ERROR IN REMOVING OLD NODES.
C                            2   , ERROR IN CREATING NEW NODES.
C                            3   , ERROR IN ADDING VALIDATION ENTRIES.
C                            4   , ERROR IN ADDING FAMILY ATTRIBUTE.
C                            5   , ERROR IN PASSWORD.
C                            6   , ATTEMPT TO DELETE OPERATIONAL NODE.
C                            7   , EXPERIMENT IDENTIFIER NOT FOUR
C                                  CHARACTERS.
C                            8   , DATE OF FIRST ARCHIVE NOT 6 DIGITS.
C
C     METHOD.
C     -------
C
C           ROOT NODES ARE ASSUMED TO EXIST. CACHE NODES CREATED
C           AUTOMATICALLY, BACKUP NODES IF REQUIRED.
C           DELETION OF OPERATIONAL NODES IS PROHIBITED.
C
C     EXTERNALS.
C     ----------
C
C           ECFILE
C
C     REFERENCE.
C     ----------
C
C           MARS DESIGN NOTE 30. FILE 460 24.03.88.
C
C     COMMENTS.
C     ---------
C
C           ROUTINE CONTAINS SECTIONS 0 TO 4 AND SECTION 9.
C           ADDITION OF FAMILY ATTRIBUTE NOT POSSIBLE IN FIRST VERSION.
C
C     AUTHOR.
C     -------
C
C           J. HENNESSY      ECMWF      28:06:89.
C
C     MODIFICATIONS
C     --------------
C
C           NONE.
C
C----<
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
C*    SECTION 0 . DEFINITION OF VARIABLES.
C     ----------------------------------------------------------------
C
      IMPLICIT LOGICAL     ( L, O, G )
      IMPLICIT CHARACTER*8 ( C, H, Y )
      IMPLICIT INTEGER     ( I, J, K, M, N )
C
C
      CHARACTER*4  HEXP
      CHARACTER*2  HSTR
      CHARACTER*4  HDIS
      CHARACTER*4  HTYP
      CHARACTER*4  HREP
      CHARACTER*4  HLEV
      CHARACTER*4  HARE
      CHARACTER*1  HPER
      CHARACTER*6  HFIR
      CHARACTER*6  HOWN
      CHARACTER*1  HBAK
      CHARACTER*2  HCLS
      CHARACTER*1  HFAM
      CHARACTER*1  HRUT
C
      CHARACTER*36 YLINE
C
      CHARACTER*160 YREQ
C
      CHARACTER*6  YNODE1
      CHARACTER*3  YNODE2
      CHARACTER*5  YNODE3
      CHARACTER*5  YNODE4
      CHARACTER*5  YNODE5
C
      DIMENSION IREASON(3)
C
      DIMENSION YLINE(3)
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
C*    SECTION 1 . SET INITIAL VALUES. CHECK PARAMETERS.
C     ----------------------------------------------------------------
C
 1000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'CCF1CR : SECTION 1.'
C
      KRET = 0
C
C*    CHECK THAT EXPERIMENT OR STREAM IDENTIFIER IS 4 CHARACTERS LONG.
C
      I = INDEX (HEXP,' ')
      IF (I.NE.0)
     C   THEN
             KRET = 7
             WRITE (*,9006) HEXP
             GO TO 9000
         ENDIF
C
C*    CHECK THAT FIRST ARCHIVE DATE CONSISTS OF 6 DIGITS.
C
      I = INDEX (HFIR,' ')
      IF (I.NE.0)
     C   THEN
             KRET = 8
             WRITE (*,9007) HFIR
             GO TO 9000
         ENDIF
C
C
C*    FOR COMPATIBILITY WITH PREVIOUS USER INTERFACE A FEW DEFAULT
C     VALUES HAVE TO BE SET UP, WHEN NONE ARE PROVIDED.
C
      IF (HCLS.EQ.'  ') HCLS = 'RD'
      IF (HFAM.EQ.' ')  HFAM = 'N'
      IF (HCLS.EQ.'RD') HFAM = 'N'
      IF (HCLS.EQ.'RD') HSTR = 'DA'
      IF (HRUT.EQ.' ')  HRUT = 'P'
C
C
C     CHECK THAT OWNER'S PASSWORD IS CORRECT LENGTH ( 5 FOR OPERATIONS
C     6 FOR RESEARCH).
C
      IL = INDEX (HOWN,' ')
      IF (IL.EQ.6) IL = 5
      IF (IL.EQ.0) IL = 6
      IF (IL.EQ.5.AND.HCLS.NE.'OD') KRET = 5
      IF (IL.EQ.6.AND.HCLS.NE.'RD') KRET = 5
      IF (KRET.EQ.5)
     C   THEN
             WRITE (*,9008) HOWN
             GO TO 9000
         ENDIF
C
C*    RESEARCH ARCHIVE IS DAILY ARCHIVE ONLY. OPERATIONAL ARCHIVES
C     HAVE SEVERAL STREAMS, WHICH ARE USED TO MAKE UP CFS PATHNAME,
C     IN THE SAME NODE AS THE RESEARCH EXPERIMENT IDENTIFIER.
C
      IF (HCLS.EQ.'OD')
     C   THEN
             HEXP = 'OPER'
             IF (HSTR.EQ.'CH') HEXP = 'CHER'
             IF (HSTR.EQ.'TG') HEXP = 'TOGA'
         ENDIF
C
C*    SET UP VALUES FOR FIRST 5 NODES EG MARSR/FC/EXPV/SPGS/1989.
C
C     NODE 1.
C
      IF (HCLS.EQ.'RD'.AND.HRUT.EQ.'P') YNODE1 = '/MARSR'
      IF (HCLS.EQ.'RD'.AND.HRUT.EQ.'T') YNODE1 = '/MATSR'
C
      IF (HCLS.EQ.'OD'.AND.HRUT.EQ.'T') YNODE1 = '/MATST'
      IF (HCLS.EQ.'OD'.AND.HRUT.EQ.'O') YNODE1 = '/MARSO'
      IF (HCLS.EQ.'OD'.AND.HRUT.EQ.'P') YNODE1 = '/MARST'
C
C     NODE 2.
C
      YNODE2 = '/'//HTYP(1:2)
C
C     NODE 3.
C
      YNODE3 = '/'//HEXP
C
C     NODE 4.
C
      YNODE4 = '/'//HREP(1:1)//HLEV(1:1)//HARE(1:1)//HPER
C
C     NODE 5.
C
      YNODE5 = '/19'//HFIR(1:2)
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
C*    SECTION 2 . REMOVE ANY EXISTING NODES AND FILES, IF REQUIRED.
C     ----------------------------------------------------------------
C
 2000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'CCF1CR : SECTION 2.'
C
      IF (HDIS(1:1).EQ.'O')
     C   THEN
C
C*           REMOVAL OF OPERATIONAL NODES AND FILES NOT ALLOWED.
C
             IF (HCLS.EQ.'OD'.AND.(HRUT.EQ.'P'.OR.HRUT.EQ.'O'))
     C          THEN
                    KRET = 6
                    WRITE (*,9004)
                    GO TO 9000
                ENDIF
C
             YTEMP = 'REMOVE'
             CALL LDC1CR (YTEMP,HOWN(1:IL),HEXP,HTYP,HCLS,HSTR,HRUT,
     C                     KPR,IRC)
C
C            IGNORE RETURN CODE FOR EXPERIMENT DOES NOT EXIST.
C
             IF (IRC.GT.1)
     C          THEN
                    KRET = 1
                    WRITE (*,9005)
                    GO TO 9000
                ENDIF
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
C*    SECTION 3 . CREATE NODE (AND BACKUP NODES IF REQUIRED).
C     ----------------------------------------------------------------
C
 3000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'CCF1CR : SECTION 3.'
C
      DO 3900 J3900 = 1 , 4
C
C*    IF FIRST TIME THROUGH SET UP /ROOT/TYPE NODES.
C
      IF (J3900.EQ.1)
     C   THEN
             YLINE(1) = YNODE1//YNODE2//','
             YLINE(2) = '/CACHE'//YNODE1//YNODE2//','
             YLINE(3) = '/MARSBACKUP'//YNODE1//YNODE2//','
         ENDIF
C
C*    IF SECOND TIME THROUGH SET UP /ROOT/TYPE/EXPVER NODES.
C
      IF (J3900.EQ.2)
     C   THEN
             YLINE(1) = YNODE1//YNODE2//YNODE3//','
             YLINE(2) = '/CACHE'//YNODE1//YNODE2//YNODE3//','
             YLINE(3) = '/MARSBACKUP'//YNODE1//YNODE2//YNODE3//','
         ENDIF
C
C*    IF THIRD TIME THROUGH SET UP /ROOT/TYPE/EXPVER/DESCRIPTOR NODES.
C
      IF (J3900.EQ.3)
     C   THEN
             YLINE(1) = YNODE1//YNODE2//YNODE3//YNODE4//','
             YLINE(2) = '/CACHE'//YNODE1//YNODE2//YNODE3//YNODE4//','
             YLINE(3) = '/MARSBACKUP'//YNODE1//YNODE2//YNODE3//YNODE4
     C                     //','
         ENDIF
C
C*    IF FOURTH TIME THROUGH SET UP /ROOT/TYPE/EXPVER/DESCRIPTOR/YEAR
C     NODES.
C
      IF (J3900.EQ.4)
     C   THEN
             YLINE(1) = YNODE1//YNODE2//YNODE3//YNODE4//YNODE5//','
             YLINE(2) = '/CACHE'//YNODE1//YNODE2//YNODE3//YNODE4
     C                     //YNODE5//','
             YLINE(3) = '/MARSBACKUP'//YNODE1//YNODE2//YNODE3//YNODE4
     C                     //YNODE5//','
         ENDIF
C
C
      J = 0
C
 3100 CONTINUE
C
      J = J + 1
      I = INDEX (YLINE(J),',')
      YREQ = 'FN=ADD,NA,PATH='//YLINE(J)(1:I)//'UVAL=UV1/WMB/'
     C          //HOWN(1:IL)//'/S.'
      IF (KPR.EQ.1) WRITE (*,9003) YREQ
      CALL ECFILE (IERR,IREASON,YREQ)
C
C     TRAP RETURN CODES FOR NODE ALREADY EXISTS.
C
      IF (IERR .EQ. 9.
     C   AND.IREASON(1).EQ.1.
     C   AND.IREASON(2).EQ.2.
     C   AND.IREASON(3).EQ.12) GO TO 3300
C
      IF (IERR.NE.0)
     C   THEN
             WRITE (*,9001) IERR , IREASON
             WRITE (*,9003) YREQ
             KRET = 2
             GO TO 9000
         ENDIF
C
C     ADD VALIDATION ENTRY.
C
      YREQ = 'FN=MODIFY,PW='//HOWN(1:IL)//',NA,PATH='//YLINE(J)(1:I)//
     C             'AVAL=PUB/IR/-/S.'
      IF (KPR.EQ.1) WRITE (*,9003) YREQ
      CALL ECFILE (IERR,IREASON,YREQ)
C
C     TRAP RETURN CODES FOR ENTRY ALREADY EXISTS.
C
      IF (IERR .EQ. 9.
     C   AND.IREASON(1).EQ.1.
     C   AND.IREASON(2).EQ.115.
     C   AND.IREASON(3).EQ.5) GO TO 3300
C
      IF (IERR.NE.0)
     C   THEN
             WRITE (*,9002) IERR , IREASON
             WRITE (*,9003) YREQ
             KRET = 3
             GO TO 9000
         ENDIF
C
 3300 CONTINUE
C
C     CREATE CACHE NODES.
C
      IF (J.LT.2) GO TO 3100
C
C     CREAT BACKUP NODES IF REQUIRED.
C
      IF (HBAK.EQ.'1'.AND.J.EQ.2) GO TO 3100
C
 3900 CONTINUE
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
C
C*    SECTION 4 . ADD FAMILY ATTRIBUTES.
C    ----------------------------------------------------------------
C
 4000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'CCF1CR : SECTION 4.'
C
      IF (HFAM.EQ.'Y')
     C   THEN
             KRET = 4
             WRITE (*,*) 'CCF1CR : FAMILY ATTRIBUTES NOT IMPLEMENTED.'
             GO TO 9000
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
C
C
C*    SECTION 9 . RETURN TO CALLING ROUTINE. FORMAT STATEMENTS.
C     ------------------------------------------------------------------
C
 9000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'CCF1CR : SECTION 9.'
C
C
      RETURN
C
C
 9001 FORMAT (1H ,'CCF1CR : NODE CREATION ERROR. FAILURE CODE = ',I3,
     C            ' , REASON CODES = ',3I6,' .')
C
 9002 FORMAT (1H ,'CCF1CR : VALIDATION ENTRY. FAILURE CODE = ',I3,
     C            ' , REASON CODES = ',3I6,' .')
C
 9003 FORMAT (1H ,A)
C
 9004 FORMAT (1H ,'CCF1CR : OPERATIONAL NODE DELETION ILLEGAL.')
C
 9005 FORMAT (1H ,'CCF1CR : DELETION OF OLD NODES/FILES FAILED.')
C
 9006 FORMAT (1H ,'CCF1CR : EXPERIMENT IDENTIFIER NOT 4 CHARACTERS - ',
     C             A)
C
 9007 FORMAT (1H ,'CCF1CR : FIRST ARCHIVE DATE NOT 6 DIGITS - ',A)
C
 9008 FORMAT (1H ,'CCF1CR : OWNER (PASSWORD) INVALID - ',A)
C
C     ------------------------------------------------------------------
C
C
      END
