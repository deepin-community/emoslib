C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE ANALSW(IBLOCKS,LOGICS,ILENGL,INTEGS,ILENGI,
     S                  REALSW,ILENGR)
C---->
C
C     G.K.SAKELLARIDES  ECMWF        15/10/85.
C
C     PURPOSE.
C     --------
C
C          TO GET AS OUTPUT THE NAME AND CONTENT OF VARIABLES WHICH
C          APPEAR IN THE DECODED MARS MODEL SWITCH RECORD
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *ALALSW(IBLOCKS,LOGICS,ILENGL,INTEGS,ILENGI,REALSW,
C                        ILENGR)
C        WHERE:
C              INPUT PARAMETERS
C              1.*IBLOCKS* INTEGER ARRAY OF LENGTH 29 CONTAINING
C                          THE FOLLOWING INFORMATIONS OF BLOCK1.
C              2.*LOGICS* LOGICAL ARRAY  WHICH CONTAINS THE LOGICALS.
C              3.*ILENGL* ACTUAL LENGTH OF LOGICALS
C              4.*INSOUT* INTEGER ARRAY WHICH CONTAINS THE INTEGER
C              5.*ILENGI* ACTUAL LENGTH OF INTEGER
C              6.*REALSW* REAL ARRAY WHICH CONTAINS THE REALS.
C              7.*ILENGR* ACTUAL LENGTH OF REALS
C
C     EXTERNALS.
C     ----------
C
C         *NONE*
C----<
C
      CHARACTER *45,Y1(26),YL(26)
      CHARACTER *12,YY
      CHARACTER *45,YI1(6),YI2,YI3(14),YI4,YI5,YI6(10),YI7,YI8,YI9
      CHARACTER *45,YR1(2),YR2,YR3(53),YR4,YR5,YR6,YR7,YR8(26)
      DIMENSION INTEGS(*),REALSW(*),IBLOCKS(*)
      LOGICAL LOGICS(*)
      DATA YY/'   POSITION='/
      DATA (Y1(I),I=1,14)
     D       /'               G                            =',
     D        '               R                            =',
     D        '               I                            =',
     D        '               B                            =',
     D        'IDENTIFICATION OF CENTRE.                   =',
     D        'MODEL IDENTIFICATION                        =',
     D        'GRID DEFINITION                             =',
     D        'FLAG (CODE TABLE 1)                         =',
     D        'PARAM. INDICATING THE MARS MODEL SWITCH REC.=',
     D        'TYPE OF LEVEL (CODE TABLE 3)                =',
     D        'VALUE 1 OF LEVEL (CODE TABLE 3)             =',
     D        'VALUE 2 OF LEVEL (CODE TABLE 3)             =',
     D        'YEAR    OF INITIAL DATA                     =',
     D        'MONTH   OF INITIAL DATA                     ='/
      DATA (Y1(I),I=15,26)
     D       /'DAY     OF INITIAL DATA                     =',
     D        'HOUR    OF INITIAL DATA                     =',
     D        'MINUTE  OF INITIAL DATA                     =',
     D        'TIME UNIT (CODE TABLE 4)                    =',
     D        'TIME RANGE 1                                =',
     D        'TIME RANGE 2                                =',
     D        'TIME RANGE FLAG (CODE TABLE 5)              =',
     D        'LENGTH OF DATA BLOCK (OCTET)                =',
     D        'STARTING ADDRESS OF LOGICAL SECTION(OCTET)  =',
     D        'STARTING ADDRESS OF INTEGER SECTION(OCTET)  =',
     D        'STARTING ADDRESS OF REAL    SECTION(OCTET)  =',
     D        'LENGTH           OF REAL    SECTION(OCTET)  ='/
      DATA (YL(I),I=1,15)
     D       /'LZLS       LOGICAL FROM COMMON *COMDIZ*     =',
     D        'LSIMDT     LOGICAL FROM COMMON *COMDSW*     =',
     D        'LSIMZQ     LOGICAL FROM COMMON *COMDSW*     =',
     D        'LVTMPC1    LOGICAL FROM COMMON *COMDSW*     =',
     D        'LVTMPC2    LOGICAL FROM COMMON *COMDSW*     =',
     D        'LLDIFF     LOGICAL FROM COMMON *COMNMI*     =',
     D        'LLROSS     LOGICAL FROM COMMON *COMNMI*     =',
     D        'LLPROJ     LOGICAL FROM COMMON *COMNMI*     =',
     D        'LLZA       LOGICAL FROM COMMON *COMNMI*     =',
     D        'LLPSRS     LOGICAL FROM COMMON *COMNMI*     =',
     D        'LLTEND     LOGICAL FROM COMMON *COMNMI*     =',
     D        'LLDIAB     LOGICAL FROM COMMON *COMNMI*     =',
     D        'LLFILT     LOGICAL FROM COMMON *COMNMI*     =',
     D        'LPHYS      LOGICAL FROM COMMON *COMPSW*     =',
     D        'LVDIFF     LOGICAL FROM COMMON *COMPSW*     ='/
      DATA (YL(I),I=16,26)
     D       /'LKUO       LOGICAL FROM COMMON *COMPSW*     =',
     D        'LCOND      LOGICAL FROM COMMON *COMPSW*     =',
     D        'LQNEGAT    LOGICAL FROM COMMON *COMPSW*     =',
     D        'LSURF      LOGICAL FROM COMMON *COMPSW*     =',
     D        'LSCV       LOGICAL FROM COMMON *COMPSW*     =',
     D        'LKUO0      LOGICAL FROM COMMON *COMPSW*     =',
     D        'LESFT      LOGICAL FROM COMMON *COMPSW*     =',
     D        'LEVAP      LOGICAL FROM COMMON *COMPSW*     =',
     D        'LSNRN      LOGICAL FROM COMMON *COMPSW*     =',
     D        'LRAD       LOGICAL FROM COMMON *COMRSW*     =',
     D        'LDIUR      LOGICAL FROM COMMON *COMRSW*     ='/
      DATA YI1/'NCYCLE     INTEGER FROM COMMON *COMDOC*     =',
     D         'NVERS      INTEGER FROM COMMON *COMDOC*     =',
     D         'NM         INTEGER FROM COMMON *COMCTL*     =',
     D         'NN         INTEGER FROM COMMON *COMCTL*     =',
     D         'NK         INTEGER FROM COMMON *COMCTL*     =',
     D         'NGL        INTEGER FROM COMMON *COMCTL*     ='/
      DATA YI2/'NLON( )    INTEGER FROM COMMON *COMCTL*     ='/
      DATA YI3/'NLEV       INTEGER FROM COMMON *COMCTL*     =',
     D         'NVCLEV     INTEGER FROM COMMON *COMCTL*     =',
     D         'NUMZLS     INTEGER FROM COMMON *COMDIZ*     =',
     D         'NFRZLS     INTEGER FROM COMMON *COMDIZ*     =',
     D         'NDTSCVL    INTEGER FROM COMMON *COMDIZ*     =',
     D         'NDTSCVS    INTEGER FROM COMMON *COMDIZ*     =',
     D         'NDQSCVL    INTEGER FROM COMMON *COMDIZ*     =',
     D         'NDQSCVS    INTEGER FROM COMMON *COMDIZ*     =',
     D         'NDUSCVL    INTEGER FROM COMMON *COMDIZ*     =',
     D         'NDUSCVS    INTEGER FROM COMMON *COMDIZ*     =',
     D         'NDVSCVL    INTEGER FROM COMMON *COMDIZ*     =',
     D         'NDVSCVS    INTEGER FROM COMMON *COMDIZ*     =',
     D         'NDESCVL    INTEGER FROM COMMON *COMDIZ*     =',
     D         'NDESCVS    INTEGER FROM COMMON *COMDIZ*     ='/
      DATA YI4/'NLANDP( )  INTEGER FROM COMMON *COMDIZ*     ='/
      DATA YI5/'NSEAP( )   INTEGER FROM COMMON *COMDIZ*     ='/
      DATA YI6/'NMASKA     INTEGER FROM COMMON *COMMSK*     =',
     D         'NMASKV     INTEGER FROM COMMON *COMMSK*     =',
     D         'NFRMSK     INTEGER FROM COMMON *COMMSK*     =',
     D         'NFRMSKP    INTEGER FROM COMMON *COMMSK*     =',
     D         'NRSLT      INTEGER FROM COMMON *COMMSK*     =',
     D         'NITNINI    INTEGER FROM COMMON *COMNMI*     =',
     D         'NDIFFST    INTEGER FROM COMMON *COMNMI*     =',
     D         'NVM        INTEGER FROM COMMON *COMNMI*     =',
     D         'NRADFR     INTEGER FROM COMMON *COMRSW*     =',
     D         'NRINT      INTEGER FROM COMMON *COMRSW*     ='/
      DATA YI7/'NTRM( )    INTEGER FROM COMMON *COMTRU*     ='/
      DATA YI8/'NTRN( )    INTEGER FROM COMMON *COMTRU*     ='/
      DATA YI9/'NTRK( )    INTEGER FROM COMMON *COMTRU*     ='/
      DATA YR1/'DTIME      REAL    FROM COMMON *COMCTL*     =',
     D         'EPS        REAL    FROM COMMON *COMCTL*     ='/
      DATA YR2/'VCT( )     REAL    FROM COMMON *COMCTL*     ='/
      DATA (YR3(I),I=1,15)
     D        /'CDIATS     REAL    FROM COMMON *COMDIA*     =',
     D         'CDIATD     REAL    FROM COMMON *COMDIA*     =',
     D         'CDIAWD     REAL    FROM COMMON *COMDIA*     =',
     D         'GPEO       REAL    FROM COMMON *COMDIA*     =',
     D         'GKEO       REAL    FROM COMMON *COMDIA*     =',
     D         'GQMO       REAL    FROM COMMON *COMDIA*     =',
     D         'GTSO       REAL    FROM COMMON *COMDIA*     =',
     D         'GTDO       REAL    FROM COMMON *COMDIA*     =',
     D         'GWSO       REAL    FROM COMMON *COMDIA*     =',
     D         'GWDO       REAL    FROM COMMON *COMDIA*     =',
     D         'GSNO       REAL    FROM COMMON *COMDIA*     =',
     D         'DSRADO     REAL    FROM COMMON *COMDIA*     =',
     D         'DTRADO     REAL    FROM COMMON *COMDIA*     =',
     D         'DSRADS     REAL    FROM COMMON *COMDIA*     =',
     D         'DTRADS     REAL    FROM COMMON *COMDIA*     ='/
      DATA (YR3(I),I=16,30)
     D        /'DVDIS      REAL    FROM COMMON *COMDIA*     =',
     D         'DHFS       REAL    FROM COMMON *COMDIA*     =',
     D         'DEVAP      REAL    FROM COMMON *COMDIA*     =',
     D         'DCVFR      REAL    FROM COMMON *COMDIA*     =',
     D         'DCVQAC     REAL    FROM COMMON *COMDIA*     =',
     D         'DCVMOI     REAL    FROM COMMON *COMDIA*     =',
     D         'DCVGR      REAL    FROM COMMON *COMDIA*     =',
     D         'DCVGS      REAL    FROM COMMON *COMDIA*     =',
     D         'DCVMS      REAL    FROM COMMON *COMDIA*     =',
     D         'DCVER      REAL    FROM COMMON *COMDIA*     =',
     D         'DCVES      REAL    FROM COMMON *COMDIA*     =',
     D         'DLSGR      REAL    FROM COMMON *COMDIA*     =',
     D         'DLSGS      REAL    FROM COMMON *COMDIA*     =',
     D         'DLSMS      REAL    FROM COMMON *COMDIA*     =',
     D         'DLSER      REAL    FROM COMMON *COMDIA*     ='/
      DATA (YR3(I),I=31,45)
     D        /'DLSES      REAL    FROM COMMON *COMDIA*     =',
     D         'DSSRAD     REAL    FROM COMMON *COMDIA*     =',
     D         'DSTRAD     REAL    FROM COMMON *COMDIA*     =',
     D         'DSHFL      REAL    FROM COMMON *COMDIA*     =',
     D         'DSDTFL     REAL    FROM COMMON *COMDIA*     =',
     D         'DSLSR      REAL    FROM COMMON *COMDIA*     =',
     D         'DSLSS      REAL    FROM COMMON *COMDIA*     =',
     D         'DSCVR      REAL    FROM COMMON *COMDIA*     =',
     D         'DSCVS      REAL    FROM COMMON *COMDIA*     =',
     D         'DSEVW      REAL    FROM COMMON *COMDIA*     =',
     D         'DSEVI      REAL    FROM COMMON *COMDIA*     =',
     D         'DSDWFL     REAL    FROM COMMON *COMDIA*     =',
     D         'DSSNMT     REAL    FROM COMMON *COMDIA*     =',
     D         'DDCTFL     REAL    FROM COMMON *COMDIA*     =',
     D         'DDCWFL     REAL    FROM COMMON *COMDIA*     ='/
      DATA (YR3(I),I=46,53)
     D        /'DSROS      REAL    FROM COMMON *COMDIA*     =',
     D         'DSROD      REAL    FROM COMMON *COMDIA*     =',
     D         'DADCON     REAL    FROM COMMON *COMDIA*     =',
     D         'DIFVO      REAL    FROM COMMON *COMHDI*     =',
     D         'DIFD       REAL    FROM COMMON *COMHDI*     =',
     D         'DIFT       REAL    FROM COMMON *COMHDI*     =',
     D         'DIFQ       REAL    FROM COMMON *COMHDI*     =',
     D         'CDRAG      REAL    FROM COMMON *COMHDI*     ='/
      DATA YR4/'ALATN( )   REAL    FROM COMMON *COMMSK*     ='/
      DATA YR5/'ALATS( )   REAL    FROM COMMON *COMMSK*     ='/
      DATA YR6/'ALONE( )   REAL    FROM COMMON *COMMSK*     ='/
      DATA YR7/'ALONW( )   REAL    FROM COMMON *COMMSK*     ='/
      DATA (YR8(I),I=1,15)
     D        /'DTINIT )   REAL    FROM COMMON *COMNMI*     =',
     D         'DTDIFF )   REAL    FROM COMMON *COMNMI*     =',
     D         'PHYFIL     REAL    FROM COMMON *COMNMI*     =',
     D         'WSMAX      REAL    FROM COMMON *COMPSW*     =',
     D         'BETADT     REAL    FROM COMMON *COMSIM*     =',
     D         'BETAZQ     REAL    FROM COMMON *COMSIM*     =',
     D         'APR        REAL    FROM COMMON *COMSIM*     =',
     D         'TR         REAL    FROM COMMON *COMSIM*     =',
     D         'VCRIT      REAL    FROM COMMON *COMSIM*     =',
     D         'HDAMP      REAL    FROM COMMON *COMSIM*     =',
     D         'GVO        REAL    FROM COMMON *COMSTA*     =',
     D         'GD         REAL    FROM COMMON *COMSTA*     =',
     D         'GQ         REAL    FROM COMMON *COMSTA*     =',
     D         'GT         REAL    FROM COMMON *COMSTA*     =',
     D         'GPS        REAL    FROM COMMON *COMSTA*     ='/
      DATA (YR8(I),I=16,26)
     D        /'GKE        REAL    FROM COMMON *COMSTA*     =',
     D         'GPE        REAL    FROM COMMON *COMSTA*     =',
     D         'GTE        REAL    FROM COMMON *COMSTA*     =',
     D         'GLQ        REAL    FROM COMMON *COMSTA*     =',
     D         'GTPE       REAL    FROM COMMON *COMSTA*     =',
     D         'GQM        REAL    FROM COMMON *COMSTA*     =',
     D         'GTS        REAL    FROM COMMON *COMSTA*     =',
     D         'GTD        REAL    FROM COMMON *COMSTA*     =',
     D         'GWS        REAL    FROM COMMON *COMSTA*     =',
     D         'GWD        REAL    FROM COMMON *COMSTA*     =',
     D         'GSN        REAL    FROM COMMON *COMSTA*     ='/
C
C*               1. LOCAL VARIABLES
C
  100 CONTINUE
      INGL=INTEGS(6)
      INVCLEV=INTEGS(6+INGL+2)
      INLEV=INTEGS(6+INGL+1)
C      INDEX FOR INTEGERS
      J1A=1
      J1T=6
      J2A=J1T+1
      J2T=J2A+INGL-1
      J3A=J2T+1
      J3T=J3A+14-1
      J4A=J3T+1
      J4T=J4A+INGL-1
      J5A=J4T+1
      J5T=J5A+INGL-1
      J6A=J5T+1
      J6T=J6A+10-1
      J7A=J6T+1
      J7T=J7A+INLEV-1
      J8A=J7T+1
      J8T=J8A+INLEV-1
      J9A=J8T+1
      J9T=J9A+INLEV-1
C        INDEX FOR REALS
      I1A=1
      I1T=I1A+2-1
      I2A=I1T+1
      I2T=I2A+2*INVCLEV-1
      I3A=I2T+1
      I3T=I3A+53-1
      I4A=I3T+1
      I4T=I4A+32-1
      I5A=I4T+1
      I5T=I5A+32-1
      I6A=I5T+1
      I6T=I6A+32-1
      I7A=I6T+1
      I7T=I7A+32-1
      I8A=I7T+1
      I8T=I8A+26-1
      PRINT*,' J9T=',J9T,'INTL=',ILENGI,'I8T=',I8T,'REAL=',ILENGR
      IF (I8T.NE.ILENGR) THEN
         GO TO 900
      ENDIF
C
C*               2.BLOCK 0 AND 1
C
  200 CONTINUE
      IMET=0
      WRITE(*,1100)
      DO 210 I=1,29
      IF (I.EQ.22.OR.I.EQ.23.OR.I.EQ.24) THEN
         GO TO 210
      ENDIF
      IMET=IMET+1
      WRITE(*,'(1X,A,I6,1X,A,I4)') Y1(IMET),IBLOCKS(I),YY,I
  210 CONTINUE
C
C*               3.LOGICALS
C
  300 CONTINUE
      WRITE(*,1200)
      DO 310 I=1,ILENGL
      WRITE(*,'(1X,A,L6,1X,A,I4)') YL(I),LOGICS(I),YY,I
  310 CONTINUE
C
C*               4.INTEGER
C
  400 CONTINUE
      WRITE(*,1300)
      DO 410 I=J1A,J1T
      WRITE(*,'(1X,A,I6,1X,A,I4)') YI1(I),INTEGS(I),YY,I
  410 CONTINUE
      DO 420 I=J2A,J2T
      WRITE(*,'(1X,A,I6,1X,A,I4)') YI2,INTEGS(I),YY,I
  420 CONTINUE
      META3=0
      DO 430 I=J3A,J3T
      META3=META3+1
      WRITE(*,'(1X,A,I6,1X,A,I4)') YI3(META3),INTEGS(I),YY,I
  430 CONTINUE
      DO 440 I=J4A,J4T
      WRITE(*,'(1X,A,I6,1X,A,I4)') YI4,INTEGS(I),YY,I
  440 CONTINUE
      DO 450 I=J5A,J5T
      WRITE(*,'(1X,A,I6,1X,A,I4)') YI5,INTEGS(I),YY,I
  450 CONTINUE
      META6=0
      DO 460 I=J6A,J6T
      META6=META6+1
      WRITE(*,'(1X,A,I6,1X,A,I4)') YI6(META6),INTEGS(I),YY,I
  460 CONTINUE
      DO 470 I=J7A,J7T
      WRITE(*,'(1X,A,I6,1X,A,I4)') YI7,INTEGS(I),YY,I
  470 CONTINUE
      DO 480 I=J8A,J8T
      WRITE(*,'(1X,A,I6,1X,A,I4)') YI8,INTEGS(I),YY,I
  480 CONTINUE
      DO 490 I=J9A,J9T
      WRITE(*,'(1X,A,I6,1X,A,I4)') YI9,INTEGS(I),YY,I
  490 CONTINUE
C
C*               5. REALS
C
  500 CONTINUE
      WRITE(*,1400)
      DO 510 I=I1A,I1T
      WRITE(*,'(1X,A,G12.6,1X,A,I4)') YR1(I),REALSW(I),YY,I
  510 CONTINUE
      DO 520 I=I2A,I2T
      WRITE(*,'(1X,A,G12.6,1X,A,I4)') YR2,REALSW(I),YY,I
  520 CONTINUE
      METR3=0
      DO 530 I=I3A,I3T
      METR3=METR3+1
      WRITE(*,'(1X,A,G12.6,1X,A,I4)') YR3(METR3),REALSW(I),YY,I
  530 CONTINUE
      DO 540 I=I4A,I4T
      WRITE(*,'(1X,A,G12.6,1X,A,I4)') YR4,REALSW(I),YY,I
  540 CONTINUE
      DO 550 I=I5A,I5T
      WRITE(*,'(1X,A,G12.6,1X,A,I4)') YR5,REALSW(I),YY,I
  550 CONTINUE
      DO 560 I=I6A,I6T
      WRITE(*,'(1X,A,G12.6,1X,A,I4)') YR6,REALSW(I),YY,I
  560 CONTINUE
      DO 570 I=I7A,I7T
      WRITE(*,'(1X,A,G12.6,1X,A,I4)') YR7,REALSW(I),YY,I
  570 CONTINUE
      METR8=0
      DO 580 I=I8A,I8T
      METR8=METR8+1
      WRITE(*,'(1X,A,G12.6,1X,A,I4)') YR8(METR8),REALSW(I),YY,I
  580 CONTINUE
C
C*                6. RETURN
C
  600 CONTINUE
C
      RETURN
C
C
C*                9.ERRORS
C
  900 CONTINUE
      WRITE(*,9901)
      WRITE(*,'(A)') 'ERROR IN THE LENGTH OF REALS'
      CALL ENDRUN
      STOP
 9901 FORMAT('ERROR IN SUBROUTINE ANALSW')
C
C*
C
 1100 FORMAT('1',20X,'VARIABLES OF BLOCK 0 AND 1')
 1200 FORMAT('1',20X,'LOGICAL')
 1300 FORMAT('1',20X,'INTEGER')
 1400 FORMAT('1',20X,'REAL   ')
      END
