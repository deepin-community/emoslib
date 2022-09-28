C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREX_GET_NAME_UNIT(KREFERENCE,YNAME,YUNIT)
C
C**** *CREX_GET_NAME_UNIT*

C
C
C     PURPOSE.
C     --------
C          FIND NAME AND THE UNIT FOR THE PARTICULAR REFERENCE NUMBER.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *CREX_GET_NAME_UNIT(KREFERENCE,YNAME,YUNIT)*
C
C
C        INPUT : *KREFERENCE* - TABLE B REFERENCE NUMBER
C        OUTPUT:
C                *CNAMES*     -  ARRAY CONTAINING ELEMENT NAMES
C                *CUNITS*     -  ARRAY CONTAINING ELEMENT UNITS
C
C
C     METHOD.
C     -------
C
C          NONE.
C
C     EXTERNALS.
C     ----------
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
C          M. DRAGOSAVAC    *ECMWF*       15/01/2006.
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
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL=40000,
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)

C
      CHARACTER*64 YNAME
      CHARACTER*24 YUNIT
C
C
      COMMON/CREXTAB/ NCREXBR(JTAB),NCREXBS(JTAB)    , NCREXBDW(JTAB),
     1                NCREXDR(JTAB),NCREXDST(JTAB)   ,
     2                NCREXDL(JTAB),NCREXDSQ(JTAB*20),NCREXP(64,255)

C
C             NCREXBR     - table B,  table reference              array
C             NCREXBS     - table B,  scale                        array 
C             NCREXBDW    - table B,  data width                   array
C             NCREXDR     - table D,  table reference              array
C             NCREXDST    - table D,  starting pointers            array
C             NCREXDL     - table D,  lengths                      array
C             NCREXDSQ    - table D,  list of sequence descriptors array
C     
       COMMON/CREXTABC/ CREXNAME(JTAB),CREXUNIT(JTAB),CREXLST(JELEM)
C
C             CREXNAME      -  table B, ELEMENT NAME           array
C             CREXUNIT      -  table B, unit                   array
C
      CHARACTER CREXNAME*64,CREXUNIT*24
      CHARACTER*6  CREXLST
C
      ICLASS=KREFERENCE/1000
      IYYY  =KREFERENCE-ICLASS*1000+1
      ICLASS=ICLASS+1
C
      I=NCREXP(ICLASS,IYYY)
      YNAME=CREXNAME(I)
      YUNIT=CREXUNIT(I)
C
      RETURN
      END
