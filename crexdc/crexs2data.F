C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXS2DATA(KBUFL ,YBUFF ,KSUP  ,KSEC3, KELEM,
     1                      CNAMES,CUNITS,KVALS,VALUES,CVALS,KERR)
C
C**** *CREXS2DATA*
C
C
C     PURPOSE.
C     --------
C          Expand section 2 of Crex (data section)
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *CREXS2DATA(KBUFL,YBUFF,KSUP,KSEC3,KELEM,CNAMES,
C                         CUNITS,KVALS,VALUES,CVALS,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of CREX message (words)
C               *YBUFF*   -  Character string containing data
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C               *KVALS*   -  dimension of VALUES array
C        OUTPUT:
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
C               *KSEC3*   -  array containing section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data compression)
C               *CNAMES*  -  character array containing element names
C               *CUNITS*  -  character array containig units
C               *VALUES*  -  real*8 array (expanded data values)
C               *CVALS*   -  character array containing text
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C
C           NONE.
C
C
C     EXTERNALS.
C     ----------
C
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
C          MILAN DRAGOSAVAC    *ECMWF*       17/01/2003.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C--------------------------------------------------------------------------------------------
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
      CHARACTER CREXWTEN*64,CREXWTU*24
C
C
      COMMON /CREXWTC/ CREXWTEN(JELEM),CREXWTU (JELEM)
C
C             CREXWTEN    -  working table element naame
C             CREXWTU     -  working table units
C
C
C
      COMMON /CREXBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
C
      COMMON /CREXOFF/ NSIZEG(JELEM),NCREXWTRG(JWORK),
     1                 NBP,NSUBSET,OMULTI
C
C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS),CH
C
      CHARACTER*(*) YBUFF
C
      DIMENSION ILIST(JELEM),IVALUES(JELEM) 
C
C
      REAL*8 VALUES(KVALS)
      REAL*8 RVIND
      REAL*8 EPS
C
      DIMENSION KSUP(JSUP),KSEC3(JSEC3)
C
C     ------------------------------------------------------------------
C*          1.  EXPAND PRELIMINARY ITEMS FROM SECTION4.
C               ---------------------------------------
 100  CONTINUE
C
C                JCV - POINTER TO CVALS ARRAY
C                JWT - POINTER TO WORKING TABLE ARRAY
C                JNS - POINTER TO VALUES ARRAY FOR SUB-SETS.
C
      IF(KERR.GT.0) RETURN
C
      IF(NSUBSET.EQ.1) THEN
         JCV = 0
      END IF
      OREPF =.FALSE.
      IEOLD=0
C
C     ------------------------------------------------------------------
C
C*          3.  Expand the data
C               ---------------
 300  CONTINUE
C
         IF(KELEM.LT.M) THEN
            KERR=25
            CALL CREXERR(KERR)
            RETURN
         END IF
c
         JNS=NSUBSET
C
         KSEC3(4)=0
         KSUP(5)=NSIZEG(NSUBSET)
C
         CALL NEXTPRT(KBUFL,YBUFF,NBPT,NGS,KERR )
         IF(KERR.NE.0) THEN
            print*,'nextprt: error ',ierr
            RETURN
         END IF
c
         ITEMP=NBPT
         CALL NEXTSEC(KBUFL,YBUFF, ITEMP,NGS,KERR )
         IF(KERR.NE.0) THEN
            print*,'nextsec: error ',ierr
            RETURN
         END IF
c
 301     CONTINUE
C
         KSEC3(3)=JNS
         KSUP(6)=JNS
C
         JNSK=(JNS-1)*KELEM
         IF(JNSK.GT.KVALS) THEN
            KERR=14
            CALL CREXERR(KERR)
            print*,'Size of values array too small.'
            print*,'Number of subsets=',jns
            print*,'Number of elements=',M
            print*,'Kelem=',KELEM

            RETURN
         END IF
C
         DO IZ=JNSK+1,JNSK+M
         VALUES(IZ)=RVIND
         END DO
C
         DO 302 JWT=1,M
C
         JWTJNS=JWT+JNSK
C

         IF(NCREXWTDW(JWT).EQ.0) THEN
            VALUES(JWTJNS)=0.0
            GO TO 302
         END IF
C
         NBPTKEEP=NBPT
c        CALL NEXTSEP(KBUFL,YBUFF,NBPT,ITEMP,KERR)
         CALL NEXTPRT(KBUFL,YBUFF,NBPT,ITEMP,KERR )
         IF(NBPT.GE.ITEMP) GO TO 600
c
c
c        Check for next E
C
         IF(NE.EQ.1) THEN
            IE=ICHAR(YBUFF(NBPT:NBPT))-48
            IF(IE.LT.0.OR.IE.GT.9) THEN
               print*,'E leading character is not a figure',
     1                 'for ',JWT,' element.'
               IEOLD=IEOLD+1
               IF(IEOLD.EQ.10) IEOLD=0
               CALL NEXTSEP(KBUFL,YBUFF,NBPT,ITEMP,KERR)
               VALUES(JWTJNS)=RVIND
               GO TO 302
            END IF
c
            IF((IE-IEOLD).EQ.1.OR.(IE-IEOLD).EQ.-9) THEN
               IEOLD=IE
            ELSE
c              check next group
               IBPT=NBPT
               CALL NEXTSEP(KBUFL,YBUFF,IBPT,ITEMP,KERR)
               CALL NEXTPRT(KBUFL,YBUFF,IBPT,ITEMP,KERR )
               IENEXT=ICHAR(YBUFF(NBPT:NBPT))-48 
               IF((IENEXT-IEOLD).GE.2.OR.(IENEXT-IEOLD).LT.0) THEN
                   print*,JWT,' Group is missing for ',NCREXWTR(JWT),
     1                    ' element.'
                  IEOLD=IEOLD+1
                  IF(IEOLD.EQ.10) IEOLD=0
                  NBPT=NBPTKEEP
                  VALUES(JWTJNS)=RVIND
                  GO TO 302
               END IF
            END IF
            NBPT=NBPT+1
         ELSE
c              check next group
c              nbpt points to first character of next group
               IBPT=NBPT
               CALL NEXTSEP(KBUFL,YBUFF,IBPT,ITEMP,KERR)
               if((IBPT-NBPT).ne.NCREXWTDW(JWT)) THEN
                  if(NCREXWTEN(JWT).eq.658367) THEN
                     izz=NBPT+NCREXWTDW(JWT)
                     if(YBUFF(izz:izz).eq.' ') then
                        IBPT=NBPT+NCREXWTDW(JWT)   
                        go to 305
                     end if
                  end if
                  do iz=JWT-1,1,-1
                  if(NCREXWTDW(JWT).eq.NCREXWTDW(iz)) then
                     print*,iz,'Group is possibly missing ',
     1                      NCREXWTR(iz),
     2               'values replaced with missing value'
                     VALUES(iz)=rvind
                  else
                     go to 111
                  end if
                  end do
c                    
                  
 111              CONTINUE
                  IF(NCREXWTEN(JWT).EQ.658367) THEN
                     JCV=JCV+1
                     VALUES(JWTJNS)=JCV*1000+NCREXWTDW(JWT)
                     CVALS(JCV)=' '
                  ELSE
                     VALUES(JWTJNS)=RVIND
                  END IF
                  NBPT=IBPT
                  GO TO 302
                end if
         END IF
C
 305  CONTINUE
C
         IF(NCREXWTR(JWT).EQ.31011.OR.
     1      NCREXWTR(JWT).EQ.31012) OREPF=.TRUE.
C
C
C     ------------------------------------------------------------------
C*          3.1 CHARACTER DATA ?
C               ----------------
 310  CONTINUE
C
         IF(NCREXWTEN(JWT).EQ.658367) THEN
C
C*          3.2  OBTAIN CHARACTER DATA FROM DATA SECTION.
C                ----------------------------------------
 320  CONTINUE
C
            IY=NCREXWTDW(JWT)
C
            CALL EXTGRPC(KBUFL,YBUFF, NBPT,IY,CH,KERR)
            IF(KERR.NE.0) THEN
               print*,'extgrpc : problem for ',JWT,' th' 
               print*,' element', NCREXWTR(JWT)
               print*,'Element is missing'
            END IF
C
C*          3.3  MOVE CHARACTER DATA TO "CVALS".
C                -------------------------------
 330  CONTINUE
C
            IYLINE=IY/80
            IYOFF =IY-IYLINE*80
            JCVINC=IYLINE
C
            JCV = JCV + 1
            JCVW= JCV
C
            IF(IYLINE.EQ.0) THEN
               CVALS(JCVW)(1:iy)=ch(1:iY)
            ELSE
               DO 332 J=1,IYLINE
               DO 333 JJ=1,80
               CVALS(JCVW)(JJ:JJ)= CH(JJ:JJ)
 333           CONTINUE
C
               JCVW=JCVW+1
C
 332           CONTINUE
C
               DO 334 J=1,IYOFF
               CVALS(JCVW)(J:J)= CH(J:J)
 334           CONTINUE
C
            END IF
C
C*          3.4  COMPUTE POINTER VALUES TO BE STORED IN "VALUES".
C                ------------------------------------------------
 340  CONTINUE
C
            VALUES(JWTJNS) = JCV*1000 + IY
            JCV            = JCV + JCVINC
C
            GO TO 302
C
         END IF
C
C*          3.5  OBTAIN VALUE FROM DATA SECTION.
C                -------------------------------
 350  CONTINUE
C
         CALL EXTGRP(KBUFL,YBUFF, NBPT,NCREXWTDW(JWT),IVAL,KERR)
         IF(KERR.NE.0) THEN
            print*,'extgrp : error ',kerr
            RETURN
         END IF
C
C     -----------------------------------------------------------------
C*          3.6  UPDATE THE ARRAY "VALUES".
C                --------------------------
 360  CONTINUE
C
C
C        CHECK IF DATA IS MISSING
C
         IF(IVAL.EQ.NVIND) THEN
            VALUES(JWTJNS)=RVIND
         ELSE
            IF(CREXWTU(JWT)(1:4).EQ.'FLAG') then
               CALL OCTAL2DEC(IVAL,IIVAL,KERR)
               IVAL=IIVAL
            END IF
            IF(NCREXWTS(JWT).GT.0) THEN
               VALUES(JWTJNS)= IVAL/10.**NCREXWTS(JWT)
            ELSE
               IIWTS=IABS(NCREXWTS(JWT))
               VALUES(JWTJNS)= IVAL*10.**IIWTS
            END IF
         END IF
C
         CALL NEXTSEP(KBUFL,YBUFF,NBPT,ITEMP,KERR)
c
         IF(NBPT.GE.ITEMP) THEN
            OMULTI=.FALSE.
            GO TO 600
         END IF
C
 302     CONTINUE
C
         ITEMP1=NBPT
         CALL NEXTSUBSET(KBUFL,YBUFF, ITEMP1,NGS,KERR )
         IF(ITEMP1.LT.NGS) THEN
c           JNS=JNS+1
            N=JNS
            IEOLD=0
c           GO TO 301
         ELSE
            OMULTI=.FALSE.
         END IF 
C
         KSUP(7)=JCV
C
C     -----------------------------------------------------------------
C*            6.  SET SUPPLEMENTARY INFORMATION.
C                 ------------------------------
C
 600  CONTINUE
C
      NBP=NBPT
      KSUP(4)= 2
C
C     ------------------------------------------------------------------
      RETURN
      END
