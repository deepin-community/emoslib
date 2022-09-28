C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GET_UNITS(KY,KERR)
C**** *GET_UNITS*
C
C
C     PURPOSE.
C     --------
C
C          Get units for the code number KY
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *GET_UNITS(KY,KERR)*
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
C          MILAN DRAGOSAVAC    *ECMWF*       07/01/2004
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

c
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL=40000,
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)

      COMMON /CREXUT/ YUNIT,CUNITNAME,CIA5,CIA2,NCODE,NRECORDS
c
c             NCODE  - array containing unit code number
c             CUNITNAME - array containing unit name
c             CIA5 - array  containing unit CCITTIA5 name
c             CIA2 - array  containing unit CCITTIA2 name
c             NRECORDS - number of entries in the list
c
      DIMENSION NCODE(JCTEXT)
      CHARACTER*30 CUNITNAME(JCTEXT)
      CHARACTER*24 CIA5(JCTEXT)
      CHARACTER*24 CIA2(JCTEXT)
      CHARACTER*24 YUNIT
c
      KERR=0
      DO I=1,NRECORDS
        IF(NCODE(I).EQ.KY) THEN
         YUNIT=CIA5(I)
         RETURN
        END IF
      END DO
c
      KERR=47
      CALL CREXERR(KERR)
c
      RETURN
      END
