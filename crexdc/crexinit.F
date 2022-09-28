C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXINIT(KBUFL,KERR)
C
C**** *CREXINIT*
C
C
C     PURPOSE.
C     --------
C         Initialize variables and constants. 
C         Read in Units conversion table
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *CREXINIT(KBUFL,KERR)*
C
C         INPUT:
C                 *KBUFL* - Size of CREX message in bytes
C
C         OUTPUT:
C                 *KERR*  - Return error code
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
C          NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          MILAN DRAGOSAVAC    *ECMWF*       07/01/2004.
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

C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL=40000,
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
      COMMON /CREXWORK/ NBPT,NGS,NVIND,NBPTSEC2,RVIND,EPS,NE,IEOLD,
     1                  NC07YYY
C
      COMMON /CREXBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
      COMMON /CREXWT/ NCREXDWINC,NCREXSCAM,NCREXAFDW,NCREXWT ,ODREPF,
     2               NCREXSTACK(JELEM),NCREXWTEN(JELEM),
     3               NCREXWTR (JELEM),NCREXWTS (JELEM),NCREXWTRV(JELEM),
     4               NCREXWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NCREXDWINC   -  data width increment
C             NCREXSCAM    -  scale multiplier
C             NCREXAFDW    -  augmented field data width
C             NCREXWT      -  pointer to working table
C             NCREXSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             NCREXWTR     -  working table reference
C             NCREXWTS     -  working scale
C             NCREXWTRV    -  working reference value
C             NCREXWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
      COMMON /CREXUT/ YUNIT,CUNITNAME,CIA5,CIA2,NCODE,NRECORDS
C
C             NCODE  - array containing unit code number
C             CUNITNAME - array containing unit name
C             CIA5 - array  containing unit CCITTIA5 name
C             CIA2 - array  containing unit CCITTIA2 name
C             NRECORDS - number of entries in the list
C

      SAVE IFIRST
C
      DIMENSION NCODE(JCTEXT)
      CHARACTER*30 CUNITNAME(JCTEXT)
      CHARACTER*24 CIA5(JCTEXT)
      CHARACTER*24 CIA2(JCTEXT)
      CHARACTER*24 YUNIT
C
      LOGICAL OFIRST
      LOGICAL ODREPF
      REAL*8  RVIND
      REAL*8  EPS
      CHARACTER*256 CROOT
      CHARACTER*256 FILENAME
C
      INTEGER EMOSNUM
      EXTERNAL EMOSNUM
C
      FILENAME=' '
      CROOT=' '
      CALL GETENV('CREX_TABLES',CROOT)
      ILNG=INDEX(CROOT,' ')
      ILNG=ILNG-1
      IF(ILNG.EQ.1) THEN
C
C        SGI/HP/SUN CREX TABLES PATH
C
         CROOT='/home/ma/emos/tables/crex/'
C
C        ADD VERSION NUMBER
C
         ILNG=INDEX(CROOT,' ')
         IEMOSNM = EMOSNUM(1)
         WRITE(CROOT(ILNG:ILNG+5),'(I6.6)') IEMOSNM
         CROOT(ILNG+6:) = '/'
         ILNG=ILNG+6
      END IF

      NGS=KBUFL
      NE=0
      NVIND=2147483647
      RVIND=1.7D38
      EPS=1.0D-8
      M=0
      N=0
      NCREXWT=0
      ODREPF=.false.
      NCREXSCAM=0
      NCREXAFDW=0
      NCREXDWINC=0
      NC07YYY=0
      YUNIT=' '
      IFIRST=1
c
      IF(IFIRST.NE.2) THEN
C
C        Open/read table containing units associated with code numbers
C        to be used with C07YYY operator
C
         FILENAME=CROOT(1:ILNG)//'units.config.dat'
         I=INDEX(FILENAME,' ')
         I=I-1
C
         print*,'Units configuration file:'
         print*,FILENAME
         OPEN(UNIT=21,FILE=FILENAME(1:I),IOSTAT=IOS,STATUS="OLD")
         IF(IOS.NE.0) THEN
            print*,'Open error on units.config.dat'
            KERR=38
            RETURN
         END IF

         I=0
         DO I=1,JCTEXT
         READ(21,'(I3,1X,A30,A24,A24)',IOSTAT=IOS,END=100) NCODE(I),
     1                CUNITNAME(I),CIA5(I),CIA2(I)
         IF(IOS.NE.0) THEN
            WRITE(*,'(A)') 'Read error on units.config.dat'
            KERR=39
            RETURN
         END IF
C
C        WRITE(*,'(I3,1X,I3,1X,A30,A24,A24)') I, NCODE(I),
C    1                CUNITNAME(I),CIA5(I),CIA2(I)
         END DO
C
 100     CONTINUE
         NRECORDS=I-1
         IFIRST=2
         CLOSE(21)
      END IF
C
      RETURN
      END
