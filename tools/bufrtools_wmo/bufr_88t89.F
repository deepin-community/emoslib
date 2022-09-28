      PROGRAM BUFR
C
C**** *BUFR*
C
C
C     PURPOSE.
C     --------
C         Repacks Bufr multisubset data into single Bufr
C         messages creating appropriate RDB key.
C
C
C**   INTERFACE.
C     ----------
C
C          NONE.
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
C         CALL BUSEL
C         CALL BUFREX
C         CALL BUFREN
C         CALL BUPRS0
C         CALL BUPRS1
C         CALL BUPRS2
C         CALL BUPRS3
C         CALL BUPRT
C         CALL BUUKEY
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/02/95.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2=4096 ,JSEC3= 4,
     1       JSEC4=   2,JELEM=160000,JSUBS=400,JCVAL=150 ,JBUFL=512000,
#ifdef JBPW_64
     2          JBPW =  64,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200,
#else
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200,
#endif
     3          JWORK=4096000,JKEY=46,JBYTE=512000)
C
      PARAMETER (KELEM=20000)
      PARAMETER (KVALS=360000)
C 
      DIMENSION KBUFF(JBUFL)
      DIMENSION KBUFR(JBUFL)
      DIMENSION KSUP(JSUP)  ,KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION KSEC2(JSEC2),KSEC3(JSEC3),KSEC4(JSEC4)
      DIMENSION KEY  (JKEY),KREQ(2)
C
      REAL*8 VALUES(KVALS),VALUE(KVALS)
      REAL*8 RQV(KELEM)
      REAL*8 RVIND, EPS
      REAL*8 RLA,RLO,ZEN_ANG
      DIMENSION KTDLST(KELEM),KTDEXP(KELEM),KRQ(KELEM)
      DIMENSION KTDLST1(KELEM)
      DIMENSION KDATA(200)
      DIMENSION IOUT(12800)
C
      CHARACTER*256 CF(100),COUT,CFIN
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
      CHARACTER*80 YENC
      CHARACTER*256 CARG(10)
c
cs      EXTERNAL GETARG,sat_zenith_angle
      EXTERNAL sat_zenith_angle
C                                                                       
C     ------------------------------------------------------------------
C*          1. INITIALIZE CONSTANTS AND VARIABLES.
C              -----------------------------------
 100  CONTINUE
C
C     Missing value indicator
C 
      itlen=6400
      itl=0
      jz=0
      nbytes=jbpw/8
      RVIND=1.7D38
      EPS=1.0D08
      nvind=2147483647
      iobs=0
      KRQL=0
      NR=0
      KREQ(1)=0
      KREQ(2)=0
      do 102 i=1,kelem
      rqv(i)=rvind
      krq(i)=nvind
 102  continue
c
C     Input file names
C
      narg=iargc()
      if(narg.lt.4) then
         print*,'Usage -- bufr_repack -i infiles -o outfile'
         stop
      end if
      nfile=narg
c
      do 104 j=1,narg
      call getarg(j,carg(j))
 104  continue

      ii=0
      io=0
      do 105 j=1,narg
      if(carg(j).eq.'-i') then
         in=j
      elseif(carg(j).eq.'-o') then
         io=j
      end if
 105  continue
      if(io.eq.0.or.in.eq.0) then
          print*,'Usage -- bufr_repack -i infiles -o outfile'
          stop
      end if
c
      cout=carg(io+1)
c
      if(io.lt.in) then
         ist=in+1
         iend=narg
      else
         ist=in+1
         iend=io-1
      end if
c
      jj=index(cout,' ')
c
      CALL PBOPEN(IUNIT1,cout(1:jj),'w',IRET)
      IF(IRET.EQ.-1) STOP 'open failed on bufr.dat'
      IF(IRET.EQ.-2) STOP 'Invalid file name'
      IF(IRET.EQ.-3) STOP 'Invalid open mode specified'
c
      do 101 ii=ist,iend
      cfin=carg(ii) 
      iln=index(cfin,' ')
C
C*          1.2 OPEN FILE CONTAINING BUFR DATA.
C               -------------------------------
 120  CONTINUE
C
      IRET=0 
      CALL PBOPEN(IUNIT,CFIN(1:iln),'r',IRET)
      IF(IRET.EQ.-1) STOP 'open failed'
      IF(IRET.EQ.-2) STOP 'Invalid file name'
      IF(IRET.EQ.-3) STOP 'Invalid open mode specified'
c
C     ----------------------------------------------------------------- 
C*          2. SET REQUEST FOR EXPANSION.
C              --------------------------
 200  CONTINUE
C
      OPRT=.FALSE.
      OENC=.true.
      NCOM=1
      OCOMP=.FALSE.
      NR=1
      OSEC3=.FALSE.
C
C*          2.1 SET REQUEST FOR PARTIAL EXPANSION.
C               ----------------------------------
 210  CONTINUE
C
C     set variable to pack big values as missing value indicator
C
C      KPMISS=1
C      KPRUS=0
C      CALL BUPRQ(KPMISS,KPRUS)
C
C     -----------------------------------------------------------------
C*          3.  READ BUFR MESSAGE.
C               ------------------
 300  CONTINUE
C
      IERR=0
      KBUFL=0
C
      IRET=0
      CALL PBBUFR(IUNIT,KBUFF,JBYTE,KBUFL,IRET) 
      IF(IRET.EQ.-1) THEN
          go to 900
      END IF
      IF(IRET.EQ.-2) STOP 'File handling problem' 
      IF(IRET.EQ.-3) STOP 'Array too small for product'
C
      N=N+1
      ikbufl=kbufl
      KBUFL=KBUFL/nbytes+1
      IF(N.LT.NR) GO TO 300
C
C     -----------------------------------------------------------------
C*          4. EXPAND BUFR MESSAGE.
C              --------------------
 400  CONTINUE
C
      CALL BUS012(KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KSEC2,KERR)
      IF(KERR.NE.0) THEN
         PRINT*,'Error in BUS012: ',KERR
         PRINT*,' BUFR MESSAGE NUMBER ',N,' CORRUPTED.'
         KERR=0
         GO TO 300
      END IF
C
      IF(KSUP(6).GT.1) THEN
         KEL=KVALS/KSUP(6)
         IF(KEL.GT.KELEM) KEL=KELEM
      ELSE
         KEL=KELEM
      END IF
C
      CALL BUFREX(KBUFL,KBUFF,KSUP,KSEC0 ,KSEC1,KSEC2 ,KSEC3 ,KSEC4,
     1            KEL,CNAMES,CUNITS,KVALS,VALUES,CVALS,IERR)
C
      IF(IERR.NE.0) then
         if(ierr.eq.45) go to 300
         call exit(2)
      end if
      iobs=iobs+ksec3(3)
C
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
      IF(KERR.NE.0) CALL EXIT(2)
C
C*          4.1 PRINT CONTENT OF EXPANDED DATA.
C               -------------------------------
 410  CONTINUE
C
      IF(.NOT.OPRT) GO TO 500
      IF(.NOT.OSEC3) GO TO 450
C
C*          4.2 PRINT SECTION ZERO OF BUFR MESSAGE.
C               -----------------------------------
 420  CONTINUE
C

      CALL BUPRS0(KSEC0)
C
C*          4.3 PRINT SECTION ONE OF BUFR MESSAGE.
C               -----------------------------------
 430  CONTINUE
C
      CALL BUPRS1(KSEC1)
C
C
C*          4.4 PRINT SECTION TWO OF BUFR MESSAGE.
C               -----------------------------------
 440  CONTINUE
c
C              AT ECMWF SECTION 2 CONTAINS RDB KEY.
C              SO UNPACK KEY
C
      CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)
C
C              PRINT KEY
C
      CALL BUPRS2(KSUP ,KEY)
C
C*          4.5 PRINT SECTION 3 OF BUFR MESSAGE.
C               -----------------------------------
 450  CONTINUE
C
C               FIRST GET DATA DESCRIPTORS
C
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
      IF(KERR.NE.0) CALL EXIT(2)
C
C               PRINT  CONTENT
C
      IF(OSEC3) THEN
         CALL BUPRS3(KSEC3,KTDLEN,KTDLST,KTDEXL,KTDEXP,KEL,CNAMES)
      END IF
C
C*         4.6 PRINT SECTION 4 (DATA).
C              -----------------------
 460  CONTINUE
C
C          IN THE CASE OF MANY SUBSETS DEFINE RANGE OF SUBSETS
C
      IF(.NOT.OO) THEN
      WRITE(*,'(a,$)') ' STARTING SUBSET TO BE PRINTED : '
      READ(*,'(BN,I4)')   IST
      WRITE(*,'(a,$)') ' ENDING SUBSET TO BE PRINTED : '
      READ(*,'(BN,I4)')   IEND
      OO=.false.
      END IF
C
C              PRINT DATA
C
      ICODE=0
      CALL BUPRT(ICODE,IST,IEND,KEL,CNAMES,CUNITS,CVALS,
     1           KVALS,VALUES,KSUP,KSEC1,IERR)
c
c              Resolve bit maps 
C
      if(iend.gt.ksec3(3)) iend=ksec3(3)
c
      DO 461 IK=IST,IEND
C 
      CALL BUBOX(IK,KSUP,KEL,KTDEXP,CNAMES,CUNITS,KVALS,VALUES,
     1           KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU,IERR)
C
      CALL BUPRTBOX(KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU)
C
 461  CONTINUE
C
C     -----------------------------------------------------------------
C*          5. COLLECT DATA FOR REPACKING.
C              ---------------------------
 500  CONTINUE
C      
      IF(.NOT.OENC) GO TO 300
C
C               FIRST GET DATA DESCRIPTORS
C
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
      IF(KERR.NE.0) CALL EXIT(2)
C
C     -----------------------------------------------------------------
C*          6. PACK BUFR MESSAGE BACK INTO BUFR.
C              ---------------------------------
 600  CONTINUE
C
    
      KKK=0
      KBUFL=JBUFL
C
C     GET REPLICATION FACTORS
C
      KK=0
      DO 601 K=1,KSUP(5)
      IF(KTDEXP(K).EQ.31001.OR.KTDEXP(K).EQ.31002.OR.
     1   KTDEXP(K).EQ.31000) THEN
         KK=KK+1
         KDATA(KK)=NINT(VALUES(K))
      END IF
 601  CONTINUE
C
      KDLEN=2
      IF(KK.NE.0) KDLEN=KK
C
C           6.1 Convert subtype 88 into 89
C
 610  CONTINUE
C
      do i=1,KSUP(6)
      do j=1,222
         iv=j+(i-1)*222
         value(iv)=rvind
      end do
      end do
c
      do i=1,KSUP(6)
      do j=1,KSUP(5)
        ij=1+(i-1)*kel
        iv=1+(i-1)*222 
        id=nint(values(ij))
        value(iv)=values(ij)
c
        ij=3+(i-1)*kel 
        iv=6+(i-1)*222
        iyear=nint(values(ij))
        value(iv)=values(ij)
c
        ij=4+(i-1)*kel 
        iv=7+(i-1)*222
        value(iv)=values(ij)
c
        ij=5+(i-1)*kel 
        iv=8+(i-1)*222
        value(iv)=values(ij)
c
        ij=6+(i-1)*kel 
        iv=9+(i-1)*222
        value(iv)=values(ij)
c
        ij=7+(i-1)*kel 
        iv=10+(i-1)*222
        value(iv)=values(ij)
c
        ij=8+(i-1)*kel 
        iv=11+(i-1)*222
        value(iv)=values(ij)
c
        ij=9+(i-1)*kel 
        iv=12+(i-1)*222
        rla=values(ij)
        value(iv)=values(ij)
c
        ij=10+(i-1)*kel 
        iv=13+(i-1)*222
        rlo=values(ij)
        value(iv)=values(ij)
c
        ij=12+(i-1)*kel 
        iv=66+(i-1)*222
        value(iv)=values(ij)
c
        iv=14+(i-1)*222
        call sat_zenith_angle(id,iyear,rla,rlo,zen_ang)
        value(iv)=zen_ang 

      end do
      end do
c
c     set bit map
c
      do i=1,ksup(6)
      do j=109,214
        iv=j+(i-1)*222
        value(iv)=1.0
        if(j.eq.174) value(iv)=0.0
      end do
      end do
c
      do j=1,ksup(6)
        iv=215+(j-1)*222
        value(iv)=254.
        iv=216+(j-1)*222
        value(iv)=1.
        iv=217+(j-1)*222
        value(iv)=100.
        iv=220+(j-1)*222
        value(iv)=254.
        iv=221+(j-1)*222
        value(iv)=1.
        iv=222+(j-1)*222
        value(iv)=0.0
      end do
c
      ktdlst1( 1)=001007
      ktdlst1( 2)=001031
      ktdlst1( 3)=002196
      ktdlst1( 4)=002221
      ktdlst1( 5)=002222
      ktdlst1( 6)=004001
      ktdlst1( 7)=004002
      ktdlst1( 8)=004003
      ktdlst1( 9)=004004
      ktdlst1(10)=004005
      ktdlst1(11)=004006
      ktdlst1(12)=005001
      ktdlst1(13)=006001
      ktdlst1(14)=007024
      ktdlst1(15)=010002
      ktdlst1(16)=002252
      ktdlst1(17)=002023
      ktdlst1(18)=007004
      ktdlst1(19)=011001
      ktdlst1(20)=011002
      ktdlst1(21)=002197
      ktdlst1(22)=002198
      ktdlst1(23)=012193
      ktdlst1(24)=002197
      ktdlst1(25)=002198
      ktdlst1(26)=020193
      ktdlst1(27)=020194
      ktdlst1(28)=020012
      ktdlst1(29)=002197
      ktdlst1(30)=002198
      ktdlst1(31)=020193
      ktdlst1(32)=020194
      ktdlst1(33)=020012
      ktdlst1(34)=002197
      ktdlst1(35)=002198
      ktdlst1(36)=020193
      ktdlst1(37)=020194
      ktdlst1(38)=020012
      ktdlst1(39)=002197
      ktdlst1(40)=002198
      ktdlst1(41)=020193
      ktdlst1(42)=020194
      ktdlst1(43)=020012
      ktdlst1(44)=002197
      ktdlst1(45)=002198
      ktdlst1(46)=020193
      ktdlst1(47)=020194
      ktdlst1(48)=020012
      ktdlst1(49)=002197
      ktdlst1(50)=002198
      ktdlst1(51)=020193
      ktdlst1(52)=020194
      ktdlst1(53)=020012
      ktdlst1(54)=002252
      ktdlst1(55)=002199
      ktdlst1(56)=007004
      ktdlst1(57)=007004
      ktdlst1(58)=013003
      ktdlst1(59)=002252
      ktdlst1(60)=002254
      ktdlst1(61)=002251
      ktdlst1(62)=002197
      ktdlst1(63)=002198
      ktdlst1(64)=012195
      ktdlst1(65)=012196
      ktdlst1(66)=012063
      ktdlst1(67)=002252
      ktdlst1(68)=002254
      ktdlst1(69)=002251
      ktdlst1(70)=002197
      ktdlst1(71)=002198
      ktdlst1(72)=012195
      ktdlst1(73)=012196
      ktdlst1(74)=012063
      ktdlst1(75)=002252
      ktdlst1(76)=002254
      ktdlst1(77)=002251
      ktdlst1(78)=002197
      ktdlst1(79)=002198
      ktdlst1(80)=012195
      ktdlst1(81)=012196
      ktdlst1(82)=012063
      ktdlst1(83)=002252
      ktdlst1(84)=002254
      ktdlst1(85)=002251
      ktdlst1(86)=002197
      ktdlst1(87)=002198
      ktdlst1(88)=012195
      ktdlst1(89)=012196
      ktdlst1(90)=012063
      ktdlst1(91)=002252
      ktdlst1(92)=002254
      ktdlst1(93)=002251
      ktdlst1(94)=002197
      ktdlst1(95)=002198
      ktdlst1(96)=012195
      ktdlst1(97)=012196
      ktdlst1(98)=012063
      ktdlst1(99)=002252
      ktdlst1(100)=002254
      ktdlst1(101)=002251
      ktdlst1(102)=002197
      ktdlst1(103)=002198
      ktdlst1(104)=012195
      ktdlst1(105)=012196
      ktdlst1(106)=012063
      ktdlst1(107)=222000
      ktdlst1(108)=236000
      ktdlst1(109)=101106
      ktdlst1(110)=031031
      ktdlst1(111)=001031
      ktdlst1(112)=001032
      ktdlst1(113)=101001
      ktdlst1(114)=033007
      ktdlst1(115)=222000
      ktdlst1(116)=237000
      ktdlst1(117)=001031
      ktdlst1(118)=001032
      ktdlst1(119)=101001
      ktdlst1(120)=033252
c
      ktdlen=120
C
C*          6.2 ENCODE DATA INTO BUFR MESSAGE.
C               ------------------------------
 620  CONTINUE
C
C     repacking multisubset data one by one
C
      CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)
c
      IF(KSUP(6).EQ.0) THEN
         print*,'Zero subsets'
         call exit(2)
      END IF
c
      key(3)=89
      ksec1(7)=89
      ksec1(15)=13
c
c     Get information for RDB key
c
      CALL BUCREKEY(KTDEXP,KSUP,KSEC1,KEY,VALUES,CVALS,KERR)
      IF(KERR.NE.0) THEN
         print*,'Error in BUCREKEY.'
         call exit(2)
      end if
c
c     Pack new RDB key
c
      CALL BUPKEY(KEY,KSEC1,KSEC2,KERR)
      IF(KERR.NE.0) CALL EXIT(2)
c
         
      kel1=222
      if(ksec3(4).eq.0) ksec3(4)=192
      CALL BUFREN( KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
     1             KTDLEN,KTDLST1,KDLEN,KDATA,KEL1,        !ksup(5),
     2             KVALS,VALUE,CVALS,KBUFL,KBUFR,KERR)
C
      IF(KERR.GT.0) THEN
         PRINT*,'ERROR DURING ENCODING.'
         CALL EXIT(2)
      END IF
C
C           6.3 WRITE PACKED BUFR MESSAGE INTO FILE.
C               ------------------------------------
 630  CONTINUE
C
      ikbufl=KBUFL*4
      CALL PBWRITE(IUNIT1,kbufr,ikbufl,IERR)
      IF(IERR.LT.0) THEN
         print*,'Error writing into target file.'
         call exit(2)
      END IF
C
      GO TO 300
C     -----------------------------------------------------------------
C
 810  CONTINUE
C
      WRITE(*,'(1H ,A)') 'OPEN ERROR ON INPUT FILE'
      GO TO 900
C      
 800  CONTINUE
C
      IF(IRET.EQ.-1) THEN
         print*,'Number of records processed ',n
      ELSE
         print*,' BUFR : error= ',ierr
      END IF
C
 900  CONTINUE
C
      CALL PBCLOSE(IUNIT,IRET)
 101  CONTINUE
c
      CALL PBCLOSE(IUNIT1,IRET)
C
      END
      SUBROUTINE SAT_ZENITH_ANGLE(SAT_ID,YEAR,OBS_LAT,OBS_LON,ZEN_ANG)


       real*8 obs_lat,obs_lon
       integer sat_id,year
       real*8 zen_ang

       parameter ( dtr    = 0.017453292)
       parameter ( radius = 6371.0)
       parameter ( pi     = 3.14159)
       parameter ( height = 35800.0)

       real  sat_lat_rad,sat_lon_rad, sat_lat, sat_lon
       real  obs_lat_rad,obs_lon_rad
       real  xx,yy,zz
       real  arcl, distance
       real  ang, s

c      begin main routine

       sat_lat = 0.0
       sat_lon = 0.0

c     Meteosat 5 for INDOEX
       if (sat_id .eq. 52 .and. year .gt. 1997) sat_lon = 63.0

       sat_lat_rad = sat_lat * dtr
       sat_lon_rad = sat_lon * dtr

       obs_lat_rad = obs_lat * dtr
       obs_lon_rad = obs_lon * dtr

       xx = cos(obs_lat_rad)*cos(obs_lon_rad)
     1       - cos(sat_lat_rad)*cos(sat_lon_rad)
       yy = cos(obs_lat_rad)*sin(obs_lon_rad)
     1       - cos(sat_lat_rad)*sin(sat_lon_rad)
       zz = sin(obs_lat_rad) - sin(sat_lat_rad)

       arcl = 2.0 * asin(sqrt(xx*xx + yy*yy + zz*zz)/2.0)                                       
       distance = arcl*radius

       ang  = (360.0 * distance/(2.0 * pi * radius)) * dtr
       s    = sqrt(radius**2.0 + (radius+height)**2.0 - 
     1        2.0 * radius * (radius+height) * cos(ang))

       zen_ang = abs(asin((radius + height) * sin(ang)/s) / dtr )

       return                                                            
      end 
