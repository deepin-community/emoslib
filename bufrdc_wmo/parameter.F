C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2=4096,JSEC3=   4,
     1          JSEC4=2,JELEM=320000,JSUBS=400,JCVAL=150 ,JBUFL=512000,
#ifdef JBPW_64
     2          JBPW =  64,JTAB =3000,JCTAB=3000,JCTST=9000,JCTEXT=9000,
#else
     2          JBPW =  32,JTAB =3000,JCTAB=3000,JCTST=9000,JCTEXT=9000,
#endif
     3          JWORK=4096000,JKEY=46, JTMAX=10,JTCLAS=64,JTEL=255)
