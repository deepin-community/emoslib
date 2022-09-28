C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
C
C**** *CREXSEL*
C
C
C
C     PURPOSE.
C     --------
C          Returns list of Data Descriptors as in Section 1  of Crex
C          message and total/requested list of elements.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *CREXSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)*
C
C
C        OUTPUT:
C               *KTDLEN*  -  number of data descriptors in section 1
C               *KTDLST*  -  array containing data descriptors in section 1
C               *KTDEXL*  -  number of entries in list of expanded data
C                            descriptors
C               *KTDEXP*  -  array containig expanded data descriptors
C               *KERR*    -  returned error code
C
C
C     METHOD.
C     -------
C
C          NONE.
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
C
      COMMON /CREXEL/ NCREXTDLEN,NCREXTDLST(JELEM),NCREXTDEXL,
     1                NCREXTDEXP(JELEM)
C
C             NCREXTDLEN - number of Data descriptors in section 1
C             NCREXTDLST - list of Data descriptors
C             NCREXTDEXL - number of expanded Data Descriptors
C             NCREXTDEXP - list of expanded Data descriptors
C
C
      DIMENSION  KTDLST(*),KTDEXP(*)
C
C
C     -----------------------------------------------------------------

C*          1.  PUT LIST OF ELEMENTS FROM COMMON BLOCK
C               --------------------------------------
C               TO REQUESTED ARRAYS.
C               --------------------
 100  CONTINUE
C
      KERR=0
C
      KTDLEN=NCREXTDLEN
      KTDEXL=NCREXTDEXL
C
      DO 101 I=1,NCREXTDLEN
      KTDLST(I)=NCREXTDLST(I)
 101  CONTINUE
C
      DO 102 I=1,NCREXTDEXL
      KTDEXP(I)=NCREXTDEXP(I)
 102  CONTINUE
C
      RETURN
      END
