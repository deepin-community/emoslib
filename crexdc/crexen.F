C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXEN(KSEC0,KSEC1,KSEC3,
     1                  KTDLEN,CREXKTDLST,KDLEN,KDATA,KELEM,KVALS,
     2                  VALUES,CVALS,KBUFL,KBUFF,KERR)
C
C**** *CREXEN*
C
C
C     PURPOSE.
C     --------
C           Creates CREX message
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *CREXEN(KSEC0,KSEC1,KSEC3,
C                          KTDLEN,CREXKTDLST,KDLEN,KDATA,KELEM,KVALS,
C                          VALUES,CVALS,KBUFL,KBUFF,KERR)*
C
C
C
C        INPUT :
C               *KBUFL*   -  Length of CREX message (words)
C               *KELEM*   -  Integer (expected number of expanded elements)
C               *KVALS*   -  Integer (expected number of data values)
C        OUTPUT:
C
C               *KSUP*    -  Integer array of 9 words containing
C                            suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- Reserved
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- Reserved
C                         -  KSUP( 5) -- M (number of elements in values
C                                           array, first index)
C                         -  KSUP( 6) -- N (number of subsets,second index
C                                           of values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total CREX message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C
C               *KSEC0*   -  Integer array of 3 words containing
C                            CREX section 0 information
C                            KSEC0( 1)-- length of section 0 (bytes)
C                            KSEC0( 2)-- total length of CREX message (bytes)
C                            KSEC0( 3)-- CREX Edition number
C
C               *KSEC1*   -  Integer array of at least 40 words
C                            containing CREX section 1 information
C                            KSEC1( 1)-- Reserved
C                            KSEC1( 2)-- CREX Edition number
C                            KSEC1( 3)-- Reserved
C                            KSEC1( 4)-- Reserved
C                            KSEC1( 5)-- Reserved
C                            KSEC1( 6)-- CREX message type
C                            KSEC1( 7)-- Reserved
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- Reserved
C                            KSEC1(10)-- Reserved
C                            KSEC1(11)-- Reserved
C                            KSEC1(12)-- Reserved
C                            KSEC1(13)-- Reserved
C                            KSEC1(14)-- CREX Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) - KSEC1(40) -- Reserved
C
C
C                            Crex Edition 2
C
C               *KSEC1*   -  Integer array of at least 40 words
C                            containing CREX section 1 information
C                            KSEC1( 1)-- Reserved
C                            KSEC1( 2)-- CREX Edition number
C                            KSEC1( 3)-- Originating Centre ( oooo)
C                            KSEC1( 4)-- Update sequence number (uu)
C                            KSEC1( 5)-- Number of subsets (sss)
C                            KSEC1( 6)-- CREX data category (nnn)
C                            KSEC1( 7)-- International data sub-category (mmm)
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- Year (yyyy)
C                            KSEC1(10)-- Month (mm)
C                            KSEC1(11)-- Day (dd)
C                            KSEC1(12)-- Hour (hh)
C                            KSEC1(13)-- Minute (mm)
C                            KSEC1(14)-- CREX Master table (tt)
C                            KSEC1(15)-- version number of Master table used (vv)
C                            KSEC1(16)-- Originating sub-centre (ppp)
C                            KSEC1(17)-- Bufr master table version number
C                            KSEC1(18)-- Bufr version number of local table used
C                            KSEC1(19) - KSEC1(40) -- Reserved
C
C
C               *KSEC3*   -  Integer array of 4 words containing
C                            CREX section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data compression)
C
C               *CNAMES*  -  Character*64  array of KELEM containing
C                            CREX Table B element names
C               *CUNITS*  -  Character*24 array of KELEM containig
C                            CREX Table B units
C               *VALUES*  -  Real*8 array of KVALS containing expanded
C                            data values
C               *CVALS*   -  Character*80 array of KVALS containing
C                            CREX code table or CCITTIA5 CREX elements
C                            entries
C               *KERR*    -  returned error code
C
C
C
C
C     METHOD.
C     -------
C
C            NONE
C
C     EXTERNALS.
C     ----------
C
C            NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       04/02/2003.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)


      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL=40000,
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /CRWORK/ NBPT,NGS,NVIND,NBPTSEC2,RVIND,EPS,NE,IEOLD,
     1                  NC07YYY
C
      COMMON /CRWT/ NCREXDWINC,NCREXSCAM,NCREXAFDW,NCREXWT ,ODREPF,
     2             NCREXSTACK(JELEM),NCREXWTEN(JELEM),
     3             NCREXWTR (JELEM),NCREXWTS (JELEM),
     4             NCREXWTDW(JELEM)
C
C             NCREXDWINC   -  data width increment
C             NCREXSCAM    -  scale multiplier
C             NCREXAFDW    -  augmented field data width
C             NCREXWT      -  pointer to working table
C             NCREXSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             NCREXWTR     -  working table reference
C             NCREXWTS     -  working scale
C             NCREXWTDW    -  working data width
C
C
C
      COMMON /CRWTC/ CREXWTEN(JELEM),CREXWTU(JELEM)
C
C             CREXWTEN    -  working table element naame
C             CREXWTU     -  working table units
C
      CHARACTER CREXWTEN*64,CREXWTU*24
C
      COMMON /CREL/ NCREXTDLEN,NCREXTDLST(JELEM),NCREXTDEXL,
     1              NCREXTDEXP(JELEM)
C
C             NCREXTDLEN - number of Data descriptors in section 3
C             NCREXTDLST - list of Data descriptors
C             NCREXTDEXL - number of expanded Data Descriptors
C             NCREXTDEXP - list of expanded Data descriptors
C
      COMMON /CRCMOFF/ NSUBSET
C

      CHARACTER*120 YENTRY
      CHARACTER*15  FMT
      CHARACTER*6  CREXKTDLST(KELEM),CREXKTDLST0
C
      INTEGER IVALUE(KELEM)
C
      CHARACTER*4 YUSE_E
C
      INTEGER IVAL,IIVAL
c
      REAL*8 EPS
      REAL*8 RVIND
      REAL*8 VAL
C

      DIMENSION KSUP(JSUP),KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION KSEC3(JSEC3)
      DIMENSION KBUFF(KBUFL)
      DIMENSION KBUFR(JBUFL)
C
      REAL*8 VALUES(KVALS)
C
      CHARACTER*80 CVALS(KVALS),CV
C
      CHARACTER*(JBUFL*4) YOUT
C
      CHARACTER*3 crcrlf
C
      CHARACTER*4 CECMWF
C
      EQUIVALENCE(KBUFR(1),YOUT)

C     ------------------------------------------------------------------
C
      CALL GETENV('USE_E',YUSE_E)
C 
      YOUT=' '
      IPT=1
      IPT0=1
      crcrlf=char(13)//char(13)//char(10)
c
c*         1. Set Constants
c             ----------------
 100  continue
c
      KERR=0
      IF(CECMWF.NE.'ECMF') THEN
         CALL CREXIVAR(KERR)
         IPT   = 1
         IPT0  = 1
         CECMWF='ECMF'
      END IF
c
C*          1. Create Section 0 - Indicator Section
C              ------------------------------------
 110  CONTINUE
c
      YOUT(1:9)='CREX++'//crcrlf
c
c*          2. Create Section 1 - Data Description Section
c              -------------------------------------------
 200  CONTINUE
c
      IF(KSEC1(2).LT.2) THEN
      YOUT(10:10)='T'
      WRITE(YOUT(10:16),'(A1,3(i2.2))') 'T',KSEC1(14),KSEC1(2),KSEC1(15)
      YOUT(17:18)=' A'
      WRITE(YOUT(19:21),'(I3.3)',IOSTAT=IOS) KSEC1(6)
      IF(IOS.NE.0) THEN
         print*,'Internal write error.'
         KERR=33
         RETURN
      END IF
      IPT=22
      ELSE
         YOUT(10:10)='T'
         WRITE(YOUT(10:20),'(A1,5(i2.2))') 'T',KSEC1(14),KSEC1(2),
     1         KSEC1(15),KSEC1(17),KSEC1(18)
         YOUT(22:22)='A'
         WRITE(YOUT(23:28),'(2(i3.3))') KSEC1(6), KSEC1(7)
         YOUT(30:30)='P'
         WRITE(YOUT(31:38),'(i5.5,i3.3)') KSEC1(3), KSEC1(16)
         YOUT(40:40)='U'
         WRITE(YOUT(41:42),'(i2.2)') ksec1(4)
         YOUT(44:44)='S'
         WRITE(YOUT(45:47),'(i3.3)') KSEC1(5)
         YOUT(49:49)='Y'
         WRITE(YOUT(50:57),'(I4.4,2(i2.2))') KSEC1(9),KSEC1(10),
     1         KSEC1(11)
         YOUT(59:59)='H'
         WRITE(YOUT(60:63),'(2(i2.2))') KSEC1(12),KSEC1(13)

         IPT=64
      END IF
C
      DO 201 I=1,KTDLEN
      YOUT(IPT:IPT+6)=' '//CREXKTDLST(I)
      IPT=IPT+7
c
      IF((IPT-IPT0).GT.60) THEN
         YOUT(IPT:IPT+2)=crcrlf
         IPT=IPT+3
         IPT0=IPT
      END IF
c
 201  CONTINUE
c
      IF(YUSE_E.EQ.'true') THEN
         iE=1
         YOUT(IPT:IPT+6)=' E++'//crcrlf
         IPT=IPT+7
      ELSE
         YOUT(IPT:IPT+4)='++'//crcrlf
         IPT=IPT+5
      END IF
c
c*         3. Load Crex Tables
c             ----------------
 300  CONTINUE
c
      CALL CREXETAB(KSEC1,KERR)
      IF(KERR.GT.0) RETURN

c
c*         3.1 Expand Data Descriptors
c              -----------------------
c
 310  CONTINUE
c
      KPT=0
      IPT0=IPT
      NSUBSET=0
      DO 401 IX=1,KSEC3(3)
C
      NSUBSET=NSUBSET+1
C
      CALL CREXETD(KPT,KTDLEN,CREXKTDLST,KDLEN,KDATA,KSEC3,
     1             KVALS,VALUES,KELEM,KERR)
      IF(KERR.GT.0) RETURN
c
c*         4. Create Section 2 - Data Section
c             -------------------------------
 400  CONTINUE
c
C
C
      DO 402 iy=1,NCREXTDEXL
      IZ=IY+(NSUBSET-1)*KELEM
c
      IF(ABS(VALUES(IZ)-RVIND)/RVIND.gt.EPS) THEN
         if(CREXWTU(IY)(1:4).EQ.'CHAR') THEN
            ICP=NINT(VALUES(IZ)/1000)
            ICH=NINT(VALUES(IZ))-ICP*1000
            IF(YUSE_E.EQ.'true') THEN
               YOUT(IPT:IPT)=CHAR(iE+48)
               iE=iE+1
               IF(iE.GT.9) iE=0
               IPT=IPT+1
            END IF
            YOUT(IPT:IPT)=' '
            IPT=IPT+1
            IF(ICHAR(CVALS(ICP)(1:1)).EQ.255) then
               cv=' '
               do ii=1,NCREXWTDW(IY)
               cv(ii:ii)='/'
               end do 
               YOUT(IPT:IPT+NCREXWTDW(IY)-1)=cv
            else
               YOUT(IPT:IPT+NCREXWTDW(IY)-1)=CVALS(ICP)
            end if
            IPT=IPT+NCREXWTDW(IY)
            YOUT(IPT:IPT)=' '
            IPT=IPT+1
         ELSE
            IF(CREXWTU(IY)(1:4).EQ.'FLAG') THEN
               IVAL=INT(VALUES(IZ))
               CALL DEC2OCTAL(IIVAL,IVAL,IERR)
               IF(IERR.NE.0) THEN
                  print*,'Error during flag conversion'
                  print*,'iz=',iz,' ','values(iz)=',values(iz)
                  print*,'iz=',iz,' ','ival      =',ival
                  print*,'iz=',iz,' ','iival      =',iival
                  KERR=34
                  RETURN 
               END IF
               VALUES(IZ)=FLOAT(IIVAL)
c           ELSEIF(CREXWTU(IY)(1:4).eq.'C   ') THEN
c              convert Kelvin into Celsius
c              VALUES(IZ)=VALUES(IZ)-273.16
            END IF

            IF(NCREXWTS(IY).GE.0) THEN
               IVALUE(IY)=NINT(VALUES(IZ)*10.**NCREXWTS(IY))
            ELSE
               IVALUE(IY)=NINT(VALUES(IZ)/10.**IABS(NCREXWTS(IY)))
            END IF
C
c-----------------------------------------------------------------
            JZ=0
            IRES=IABS(IVALUE(IY))
            DO WHILE(IRES.GT.0)
            JZ=JZ+1
            IRES=IRES/10
            END DO
c
            IF(JZ.GT.NCREXWTDW(IY)) THEN
      print*,'Element=',iy,' NCREXWTR=',NCREXWTR(iy),'value=',ivalue(iy)
               print*,'number of figures calculated ',jz
               print*,'number of figures in tables ',NCREXWTDW(iy)
c
c              Replace this value with missing value '/'
c
               YOUT(IPT:IPT)=' '
               IPT=IPT+1
               if(YUSE_E.EQ.'true') THEN
                  YOUT(IPT:IPT)=CHAR(iE+48)
                  iE=iE+1
                  if(iE.gt.9) iE=0
                  IPT=IPT+1
               END IF
               DO III=1,NCREXWTDW(IY)
               YOUT(IPT:IPT)='/'
               IPT=IPT+1
               END DO
            ELSE
c-----------------------------------------------------------------
               IF(IVALUE(IY).LT.0) THEN
                  YOUT(IPT:IPT)=' '
                  IF(YUSE_E.eq.'true') THEN
                     IPT=IPT+1
                     YOUT(IPT:IPT)=CHAR(iE+48)
                     iE=iE+1
                     IF(iE.gt.9) iE=0
                  END IF
                  IPT=IPT+1
                  YOUT(IPT:IPT)='-'
               ELSE
                  YOUT(IPT:IPT)=' '
                  IF(YUSE_E.EQ.'true') THEN
                     IPT=IPT+1
                     YOUT(IPT:IPT)=CHAR(iE+48)
                     iE=iE+1
                     IF(iE.gt.9) iE=0
                  END IF
               END IF
c
               IPT=IPT+1
c
               FMT='(i02.02)'
               IF(NCREXWTR(iy).eq.031001.or.NCREXWTR(iy).eq.031002) then
                  NCREXWTDW(iy)=4
               END IF
               WRITE(FMT(3:4),'(I2.2)',IOSTAT=IOS) NCREXWTDW(iy)
               IF(IOS.NE.0) THEN
                  print*,'Internal write error.'
                  KERR=33
                  RETURN
               END IF
               FMT(6:7)=FMT(3:4)
               WRITE(YOUT(IPT:IPT+NCREXWTDW(iy)-1),FMT,IOSTAT=IOS)
     1               IABS(IVALUE(IY))
               IF(IOS.NE.0) THEN
                  print*,'Internal write error ',ios
                  KERR=33
                  RETURN
               END IF
               IPT=IPT+NCREXWTDW(iy)
            END IF
         END IF
      ELSE
         YOUT(IPT:IPT)=' '
         IF(YUSE_E.eq.'true') THEN
            IPT=IPT+1
            YOUT(IPT:IPT)=CHAR(iE+48)
            iE=iE+1
            IF(iE.gt.9) iE=0
         END IF
c
         IPT=IPT+1
c
         DO III=1,NCREXWTDW(iy)
          YOUT(IPT:IPT)='/'
          IPT=IPT+1
         END DO
      END IF
c
      IF((IPT-IPT0).GT.60) THEN
         YOUT(IPT:IPT+2)=crcrlf
         IPT=IPT+3
         IPT0=IPT
      END IF
C
 402  CONTINUE
c
      YOUT(IPT:IPT+3)='+'//crcrlf
      IPT=IPT+4
      IPT0=IPT
c
 401  CONTINUE
C
      IPT=IPT-4
      YOUT(IPT:IPT+4)='++'//crcrlf
      IPT=IPT+5
c
c*          5. Create Section 4 - End Section
c              -------------------------------
 500  continue
c
      YOUT(IPT:IPT+6)='7777'//crcrlf
      IPT=IPT+6
      IPT0=IPT
C
C           6. Return the size of crex in bytes
C              --------------------------------
 600  CONTINUE

      NBYTPW=JBPW/8
      K=IPT/NBYTPW+1
      DO 601 I=1,K
      KBUFF(i)=KBUFR(i)
 601  END DO
C
      KBUFL=IPT
C
      RETURN
      END
