C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE LDC1CR (HVERB,HOWN,HEXP,HTYP,HCLS,HSTR,HRUT,KPR,KRC)
C
C
C---->
C**** LDC1CR
C
C     PURPOSE.
C     --------
C
C           LIST ALL NODES AND FILES FOR A GIVEN RESEARCH EXPERIMENT.
C           FILES AND NODES UNDER OPERATIONS TEST ROOT-NODE MATST CAN
C           ALSO BE LISTED. OPERATIONAL ARCHIVES CANNOT BE LISTED -
C           THE VOLUME IS FAR TOO GREAT.
C
C           REMOVE ALL NODES AND FILES FOR A GIVEN RESEARCH EXPERIMENT.
C           FILES AND NODES UNDER OPERATIONS TEST ROOT-NODE MATST CAN
C           ALSO BE REMOVED. OPERATIONAL ARCHIVES CANNOT BE REMOVED.
C
C**   INTERFACE.
C     ----------
C
C           CALL LDC1CR (HVERB,HOWN,HEXP,HTYP,HCLS,HSTR,HRUT,KPR,KRC)
C
C
C           INTEGER    K.
C           REAL       P.
C           LOGICAL    O.
C           CHARACTER  H.
C
C               INPUT PARAMETERS.
C               -----------------
C
C               HVERB      - ACTION REQUIRED. 'LIST' OR 'REMOVE'
C
C               HOWN       - OWNER'S PASSWORD. 6 CHARACTERS FOR
C                            RESEARCH EXPERIMENT, 5 FOR OPERATIONS.
C
C               HEXP       - 4 CHARACTER EXPERIMENT IDENTIFIER.
C
C               HSTR       - ARCHIVE STREAM.
C                            FOR RESEARCH.
C                                DA - DAILY ARCHIVE.
C                            FOR OPERATIONS.
C                                DA - DAILY ARCHIVE.
C                                CH - CHERNOBYL ARCHIVE.
C                                TG - TOGA ARCHIVE.
C                                MO - MONTHLY ARCHIVE.
C
C               HTYP       - DATA TYPE.
C                            AN  - ANALYSIS.
C                            IA  - INITIALISED ANALYSIS.
C                            FG  - FIRST GUESS.
C                            FC  - FORECAST.
C                            EA  - ERRORS IN ANALYSIS.
C                            CL  - CLIMATOLOGY.
C                            ALL - ALL TYPES.
C
C               HCLS       - OD , OPERATIONS.
C                            RD , RESEARCH.
C
C               HRUT       - ROOT NODE TO BE USED.
C                            FOR RESEARCH.
C                               P , PRODUCTION ROOT MARSR.
C                               T , TEST ROOT MATSR.
C                            FOR OPERATIONS.
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
C               KRC        - ERROR RETURN CODE.
C                        0 - NO ERROR ENCOUNTERED.
C                        1 - EXPERIMENT DOES NOT EXIST
C                        2 - EXPERIMENT IDIDENTIFIER NOT 4 CHARACTERS.
C                        3 - ACTION ATTEMPTED ON OPERATIONAL NODES OR
C                            FILES.
C                        4 - ERROR REPORTED BY ECFILE.
C                        5 - OWNER PASSWORD INCORRECT LENGTH.
C                        6 - NO FREE FORTRAN UNIT NUMBERS.
C
C
C     METHOD.
C     -------
C
C           EXPERIMENT IDENTIFIER , DATA TYPE, CLASS AND ROOT ARE USED
C           TO DETERMINE NODES AND FILES, WHICH ARE THEN LISTED.
C           THE LIST IS USED TO DELETE NODES AND FILES IF REQUESTED.
C
C     EXTERNALS.
C     ----------
C
C           ECFILE
C           L2U1CR
C
C     REFERENCE.
C     ----------
C
C           NONE.
C
C     COMMENTS.
C     ---------
C
C           PROGRAM CONTAINS SECTIONS 0 TO 6 AND SECTION 9.
C
C
C     AUTHOR.
C     -------
C
C           J.HENNESSY       ECMWF      04:07:89.
C
C     MODIFICATIONS
C     --------------
C
C           NONE.
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
C*    SECTION 0 . DEFINITION OF VARIABLES. SET INITIAL VALUES.
C     ----------------------------------------------------------------
C
      IMPLICIT LOGICAL     ( L, O, G )
      IMPLICIT CHARACTER*8 ( C, H, Y )
      IMPLICIT INTEGER     ( I, J, K, M, N )
C
C
C
C
C
      DIMENSION      YPATH(3)
      DIMENSION      YIU(6)
      DIMENSION      IU(6)
      DIMENSION      IREASON(5)
C
      CHARACTER*1    YCOMMA,YSTOP
      CHARACTER*2    YIU
      CHARACTER*5    YPATHC
C
      CHARACTER*8    HVERB
      CHARACTER*6    HOWN
      CHARACTER*4    HEXP
      CHARACTER*2    HSTR
      CHARACTER*4    HTYP
      CHARACTER*2    HCLS
      CHARACTER*1    HRUT
C
      CHARACTER*7    YLFNIN
      CHARACTER*11   YLSTNA
      CHARACTER*15   YNODE
      CHARACTER*17   YPATH
      CHARACTER*80   YLINE1
      CHARACTER*80   YLINE2
      CHARACTER*80   YPATH1
      CHARACTER*80   YPATH2
C
      YLSTNA    = 'FN=LIST,NA,'
      YCOMMA    = ','
      YSTOP     = '.'
C
C     DEFAULT VALUES FOR NODES/FILES ARE RESEARCH PRODUCTION NODES AND
C     FILES.
C
      YPATHC    = 'PATH='
      YPATH(1)  = '/MARSR           '
      YPATH(2)  = '/CACHE/MARSR     '
      YPATH(3)  = '/MARSBACKUP/MARSR'
C
C     CHECK IF ACTION REQUIRED ON RESEARCH TEST ROOT.
C
      IF (HCLS.EQ.'RD'.AND.HRUT.EQ.'T')
     C   THEN
             YPATH(1)  = '/MATSR           '
             YPATH(2)  = '/CACHE/MATSR     '
             YPATH(3)  = '/MARSBACKUP/MATSR'
         ENDIF
C
C     CHECK IF ACTION REQUIRED ON OPERATIONS TEST ROOT.
C
      IF (HCLS.EQ.'OD'.AND.HRUT.EQ.'T')
     C   THEN
             YPATH(1)  = '/MATST           '
             YPATH(2)  = '/CACHE/MATST     '
             YPATH(3)  = '/MARSBACKUP/MATST'
         ENDIF
C
C     CHECK FOR ATTEMPTED ACTION ON OPERATIONS PRODUCTION ROOTS.
C
      IF (HCLS.EQ.'OD'.AND.HRUT.NE.'T')
     C   THEN
             KRC = 3
             WRITE (*,9100)
             GO TO 9000
         ENDIF
C
C     GET 6 UNUSED FORTRAN UNIT NUMBERS.
C
      DO 10 J10 =1,6
         CALL FTN1CR (IU(J10),KPR,ISERR)
         IF (ISERR.NE.0)
     C      THEN
                WRITE (*,9800)
                KRC = 6
                GO TO 9000
            ENDIF
         WRITE (YIU(J10),'(I2.2)') IU(J10)
         OPEN (UNIT=IU(J10),STATUS='SCRATCH')
   10 CONTINUE
C
C
C
      JLOOP  = 0
      KRC    = 0
      IFLAG  = 0
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
C*    SECTION 1 . PREPARE LIST COMMAND USING PATHNAME AND GET NODE1.
C     ----------------------------------------------------------------
C
 1000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'LDC1CR : SECTION 1'
C
C*    GO TO SECTION 9 IF ERROR IN EXPERIMENT IDENTIFIER .
C
      I = INDEX (HEXP,' ')
      IF (I.NE.0)
     C   THEN
             KRC = 2
             WRITE(*,9300) HEXP
             IFLAG = 1
             GO TO 9000
         ENDIF
C
C*    IF ACTION IS 'REMOVE' CHECK PASSWORD LENGTH.
C
      IF (HVERB.EQ.'REMOVE  ')
     C   THEN
             IL = INDEX (HOWN,' ')
             IF (IL.EQ.6) IL = 5
             IF (IL.EQ.0) IL = 6
             IF (IL.EQ.5.AND.HCLS.NE.'OD') KRC = 5
             IF (IL.EQ.6.AND.HCLS.NE.'RD') KRC = 5
             IF (KRC.NE.0)
     C          THEN
                    WRITE (*,9400) HOWN
                    GO TO 9000
                ENDIF
         ENDIF
C
C     LOOP ROUTINE FOR EACH TYPE OF PATHNAME
C
C     IF JLOOP = 1 LOOK FOR NODES BEGINNING WITH /MARSR
C     IF JLOOP = 2 LOOK FOR NODES BEGINNING WITH /CACHE/MARSR
C     IF JLOOP = 3 LOOK FOR NODES BEGINNING WITH /MARSBACKUP/MARSR
C
      JLOOP = JLOOP + 1
      IF (JLOOP.GT.3) GO TO 6000
C
C*    REWIND ALL FILES AT BEGINNING OF EACH LOOP
C
      REWIND IU(1)
      REWIND IU(2)
      REWIND IU(3)
      REWIND IU(4)
C
      YLFNIN        = 'L=FT' // YIU(1)
      YLINE1(1:)    = ' '
      YLINE1(1:11)  = YLSTNA
      YLINE1(12:17) = YLFNIN
      YLINE1(18:18) = YCOMMA
      YLINE1(19:23) = YPATHC
      YLINE1(24:)   = YPATH(JLOOP)
C
      IJ = INDEX(YLINE1,' ')
      YLINE1(IJ:IJ) = YSTOP
C
      IF (KPR.EQ.1) WRITE (*,'(1X,A80)') YLINE1
C
      CALL ECFILE (IERR,IREASON,YLINE1)
C
C*    TRAP CFS RETURN CODES FOR FILE/NODE DOES NOT EXIST.
C
      IF (IERR .EQ. 9.
     C   AND.IREASON(1).EQ.1.
     C   AND.IREASON(2).EQ.1.
     C   AND.IREASON(3).EQ.33) GO TO 1000
C
C*    GO TO SECTION 9 , IF ANY OTHER ERROR REPORTED.
C
      IF (IERR.NE.0)
     C   THEN
C
             KRC = 4
             WRITE (*,9500) IERR, IREASON
             WRITE (*,9600) YLINE1
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
C*    SECTION 2 . ISSUE LIST COMMAND(S) USING PATHNAME/NODE1/EXPID.
C     ---------------------------------------------------------------
C
C
 2000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'LDC1CR : SECTION 2'
C
C     CREATE EACH PATHNAME + NODE(S)
C     IF TYPE = ALL THEN LIST ALL TYPES
C
      REWIND IU(1)
C
 2100 CONTINUE
      READ (IU(1),'(A80)',END=6000) YLINE1
      CALL L2U1CR (YLINE1)
      IF (YLINE1(19:22).NE.'DESC') GO TO 2100
C
C
 2200 CONTINUE
      READ (IU(1),'(A80)',END=6000) YLINE1
      CALL L2U1CR (YLINE1)
      IF (YLINE1(19:22).EQ.'    ') GO TO 1000
C
      IF (HTYP(1:3).EQ.'ALL') GO TO 2220
      IF (HTYP(1:2).NE.YLINE1(19:20)) GO TO 2200
C
 2220 CONTINUE
      YNODE = '/' // YLINE1(19:20) // '/' // HEXP
      IF (JLOOP .EQ. 1) YPATH1 = YPATH(JLOOP)(1:6) // YNODE
      IF (JLOOP .EQ. 2) YPATH1 = YPATH(JLOOP)(1:12) // YNODE
      IF (JLOOP .EQ. 3) YPATH1 = YPATH(JLOOP) // YNODE
C
C
      YLFNIN         = 'L=FT'//YIU(2)
      YLINE1(1:11)   = YLSTNA
      YLINE1(12:17)  = YLFNIN
      YLINE1(18:18)  = YCOMMA
      YLINE1(19:23)  = YPATHC
      YLINE1(24:)    = YPATH1
      IJ = INDEX (YLINE1,' ')
      YLINE1(IJ:IJ)  = YSTOP
      IPTR = IJ
      IF (KPR.EQ.1) WRITE (*,'(1H ,A80)') YLINE1
C
      CALL ECFILE (IERR,IREASON,YLINE1)
C
C*    TRAP CFS RETURN CODES FOR FILE/NODE DOES NOT EXIST.
C
      IF (IERR.EQ.9.
     C    AND.IREASON(1).EQ.1.
     C    AND.IREASON(2).EQ.1.
     C    AND.IREASON(3).EQ.33) GO TO 2200
C
C*    GO TO SECTION 9 , IF ANY OTHER ERROR REPORTED.
C
      IF (IERR.NE.0)
     C   THEN
             KRC = 4
             WRITE (*,9500) IERR, IREASON
             WRITE (*,9600) YLINE1
             GO TO 9000
         ENDIF
C
C     SET FLAG TO INDICATE NODE FOUND FOR GIVEN EXPERIMENT IDENTIFIER.
C
      IFLAG = 1
C
C     -----------------------------------------------------------------
C
C
C
C
C
C
C
C
C*    SECTION 3 . ISSUE LIST COMMAND USING PATHNAME/NODE1/EXPID/NODE2.
C     ------------------------------------------------------------------
C
 3000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'LDC1CR : SECTION 3'
C
C     CREATE PATHNAME WITH NEXT NODE.
C
      REWIND IU(2)
 3100 CONTINUE
      READ (IU(2),'(A80)',END=2200) YLINE1
      CALL L2U1CR (YLINE1)
      IF (YLINE1(19:22).NE.'DESC')  GO TO 3100
C
      IPT1 = 0
 3200 CONTINUE
      READ (IU(2),'(A80)',END=2200) YLINE1
      CALL L2U1CR (YLINE1)
      IF (YLINE1(19:22).EQ.'    ')  GO TO 2200
      YNODE = '/'//YLINE1(19:22)
      IJ = INDEX (YPATH1,' ')
      IF (IPT1.EQ.0) IPT1 = IJ - 1
      YPATH1 = YPATH1(1:IPT1)//YNODE
C
      YLINE1(1:)     = ' '
      YLFNIN         = 'L=FT'//YIU(3)
      YLINE1(1:11)   = YLSTNA
      YLINE1(12:17)  = YLFNIN
      YLINE1(18:18)  = YCOMMA
      YLINE1(19:23)  = YPATHC
      YLINE1(24:) = YPATH1
      IJ = INDEX (YLINE1,' ')
      YLINE1(IJ:IJ)  = YSTOP
      IF (KPR.EQ.1) WRITE (*,'(1H ,A80)') YLINE1
C
      CALL ECFILE (IERR,IREASON,YLINE1)
C
C*    TRAP CFS RETURN CODES FOR FILE/NODE DOES NOT EXIST.
C
      IF (IERR.EQ.9.
     C   AND.IREASON(1).EQ.1.
     C   AND.IREASON(2).EQ.1.
     C   AND.IREASON(3).EQ.33) GO TO 3200
C
C*    GO TO SECTION 9 , IF ANY OTHER ERROR REPORTED.
C
      IF (IERR.NE.0)
     C   THEN
             KRC = 4
             WRITE (*,9500) IERR, IREASON
             WRITE (*,9600) YLINE1
             GO TO 9000
         ENDIF
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
C*    SECTION 4. ISSUE LIST COMMAND PATHNAME/NODE1/EXPID/NODE2/NODE3.
C     ------------------------------------------------------------------
C
 4000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'LDC1CR : SECTION 4'
C
      REWIND IU(3)
C
 4200 CONTINUE
      READ (IU(3),'(A80)',END=3200) YLINE1
      CALL L2U1CR (YLINE1)
      IF (YLINE1(19:22).NE.'DESC')  GO TO 4200
C
 4300 CONTINUE
      READ (IU(3),'(A80)',END=3200) YLINE1
      CALL L2U1CR (YLINE1)
      IF (YLINE1(19:22).EQ.'    ')  GO TO 3200
      YNODE = '/'//YLINE1(19:22)
      IJ = INDEX (YPATH1,' ')
      IPTR = IJ - 1
      YPATH1 = YPATH1(1:IPTR)//YNODE
      YPATH2 = YPATH1
      WRITE (*,'(1H1,''CFS PATHNAME -'')')
      WRITE (*,'(5X,A80)') YPATH1
C
C*    WRITE PATHNAME TO FILE USED IN DELETING NODES .
C
      YLINE2='FN=REMOVE,PW='//HOWN(1:IL)//',NA,PATH='//YPATH2
      IJ = INDEX (YLINE2,' ')
      YLINE2(IJ:) = '.'
      WRITE (IU(6),'(A80)') YLINE2
C
C
      REWIND IU(4)
      YLINE1(1:)     = ' '
      YLFNIN         = 'L=FT'//YIU(4)
      YLINE1(1:11)   = YLSTNA
      YLINE1(12:17)  = YLFNIN
      YLINE1(18:18)  = YCOMMA
      YLINE1(19:23)  = YPATHC
      YLINE1(24:)    = YPATH1
      IJ = INDEX (YLINE1,' ')
      YLINE1(IJ:IJ)  = YSTOP
      YPATH1(IPTR+1: ) = ' '
      IF (KPR.EQ.1) WRITE (*,'(1H ,A80)') YLINE1
C
      CALL ECFILE (IERR,IREASON,YLINE1)
C
C*    TRAP CFS RETURN CODES FOR FILE/NODE DOES NOT EXIST.
C
      IF (IERR.EQ.9.
     C   AND.IREASON(1).EQ.1.
     C   AND.IREASON(2).EQ.1.
     C   AND.IREASON(3).EQ.33) GO TO 4300
C
C*    GO TO SECTION 9 , IF ANY OTHER ERROR REPORTED.
C
      IF (IERR.NE.0)
     C   THEN
             KRC = 4
             WRITE (*,9500) IERR, IREASON
             WRITE (*,9600) YLINE1
             GO TO 9000
         ENDIF
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
C*    SECTION 5. PRINT OUT CFS FILE NAMES.
C     ---------------------------------------------------------------
C
 5000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'LDC1CR : SECTION 5.'
C
      REWIND (IU(4))
C
      WRITE (*,'(1H ,''CFS-FILES'')')
 5100 CONTINUE
      READ (IU(4),'(A80)',END=5300) YLINE1
      CALL L2U1CR (YLINE1)
      IF (YLINE1(19:22).NE.'DESC') GO TO 5100
C
 5200 CONTINUE
      READ (IU(4),'(A80)',END=5300) YLINE1
      CALL L2U1CR (YLINE1)
      IF (YLINE1(19:22).EQ.'    ') GO TO 4300
      WRITE (*,'(5X,A14)') YLINE1(19:32)
      YLINE2='FN=DELETE,PW='//HOWN(1:IL)//',NA,PATH='//YPATH2
      IJ = INDEX (YLINE2,' ')
      INOD = IJ
      YLINE2(IJ:) = '/'//YLINE1(19:32)//'.'
C
C*    WRITE PATHNAME TO FILE USED IN DELETING FILES .
C
      WRITE (IU(5),'(A80)') YLINE2
      GO TO 5200
 5300 CONTINUE
      WRITE (*,*) 'HOW DID I GET HERE'
      YLINE2 = YLINE2(1:IJ-1)//'.'
      WRITE (*,'(1H ,A80)') YLINE2
      GO TO 4300
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
C*    SECTION 6 . DELETE CFS FILES AND REMOVE NODES . IF REQUIRED.
C     ------------------------------------------------------------------
C
 6000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'LDC1CR : SECTION 6.'
C
C*    GO TO SECTION 9 IF LIST OF NODES AND FILES ONLY.
C
      IF (HVERB.EQ.'LIST    ') GO TO 9000
      IERR=0
C
C     DELETE FILES.
C
      REWIND IU(5)
 6100 CONTINUE
      READ (IU(5),'(A80)',END=6200) YLINE2
C
      IF (KPR.EQ.1) WRITE (*,'(1H ,A80)') YLINE2
      CALL ECFILE (IERR,IREASON,YLINE2)
C
C*    GO TO SECTION 9 , IF ERROR REPORTED.
C
      IF (IERR.NE.0)
     C   THEN
             KRC = 4
             WRITE (*,9700) IERR, IREASON
             WRITE (*,9600) YLINE2
             GO TO 9000
         ENDIF
C
      GO TO 6100
C
 6200 CONTINUE
C
C     REMOVE NODES.
C
      REWIND IU(6)
 6300 CONTINUE
      READ (IU(6),'(A80)',END=6400) YLINE2
 6350 CONTINUE
      IF (KPR.EQ.1) WRITE (*,'(1H ,A80)') YLINE2
      CALL ECFILE (IERR,IREASON,YLINE2)
C
C     TRAP RETURN CODES FOR NODE HAS DESCENDANTS. EXPERIMENT
C     IDENTIFIER NODES CAN HAVE SEVERAL DESCENDANTS, AND ARE
C     REMOVED WHEN ALL DESCENDANTS HAVE BEEN REMOVED.
C
      IF (IERR.EQ.9.
     C    AND.IREASON(1).EQ.1.
     C    AND.IREASON(2).EQ.104.
     C    AND.IREASON(3).EQ.0)   GO TO 6300
C
C*    GO TO SECTION 9 , IF ERROR REPORTED.
C
      IF (IERR.NE.0)
     C   THEN
             KRC = 4
             WRITE (*,9700) IERR, IREASON
             WRITE (*,9600) YLINE2
             GO TO 9000
         ENDIF
C
C     MODIFY COMMAND AS LAST NODE HAS BEEN REMOVED.
C
      I = INDEX (YLINE2,'.')
      DO 6360 J6360 = I,1,-1
         IF (YLINE2(J6360:J6360).EQ.'/')
     C      THEN
                YLINE2(J6360:) = '.'
                GO TO 6370
            ENDIF
 6360 CONTINUE
C
 6370 CONTINUE
C
C     NODES ARE REMOVED ONLY AS FAR AS THE TYPE NODE EG AN,OR FC.
C
      IF (YLINE2(J6360-3:J6360-3).NE.'/') GO TO 6350
C
      GO TO 6300
C
C
 6400 CONTINUE
C
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
C*    SECTION 9 . RETURN TO CALLING ROUTINE.FORMAT STATEMENTS.
C     ------------------------------------------------------------------
C
C
 9000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'LDC1CR : SECTION 9.'
C
C     CLOSE AND RETURN FILES.
C
      DO 9010 J9010=1,6
         CLOSE (UNIT=IU(J9010))
 9010 CONTINUE
C
C     SEE IF THE EXPERIMENT DOES EXIST IN CFS (IE IFLAG =1)
C
      IF (IFLAG .EQ. 0)
     C   THEN
          KRC = 1
          WRITE (*,9200) HEXP
         ENDIF
      RETURN
C
C
 9100 FORMAT (' LDC1CR : ACTION ATTEMPTED ON OPERATIONAL PRODUCTION',
     C          ' NODES OR FILES.')
C
 9500 FORMAT (' LDC1CR : NODE/FILE LISTING ERROR. FAILURE CODE = ',I3,
     C          ' , REASON CODES = ',3I6,'.')
C
 9600 FORMAT (1H ,A)
C
 9700 FORMAT (' LDC1CR : NODE/FILE REMOVAL ERROR. FAILURE CODE = ',I3,
C
     C          ' , REASON CODES = ',3I6,'.')
C
 9800 FORMAT (' LDC1CR : NO FREE FORTRAN UNIT NUMBERS.')
C
 9200 FORMAT (' LDC1CR : EXPERIMENT IDENTIFIER OF ',A5,' NOT IN CFS')
C
 9300 FORMAT (' LDC1CR : EXPERIMENT IDENTIFIER NOT 4 CHARACTERS - ',
     C          A)
 9400 FORMAT (' LDC1CR : OWNER (PASSWORD) INVALID - ',A)
C
C     ------------------------------------------------------------------
C
C
      END
