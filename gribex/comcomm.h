
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
C
C     Definition of network communications block used when
C     talking to MARS/IBM.
C
      PARAMETER (JPPHLN=80)
      PARAMETER (JPSHLN=30)
      PARAMETER (JPMXTR=99)
      PARAMETER (JPALEN=400)
      PARAMETER (JPCLEN=80)
      PARAMETER (JPDLEN=1520)
c----      PARAMETER (JPDLEN=960)
      PARAMETER (JPMAXV=30)
C
      CHARACTER*(JPCLEN) YBLOCK
      CHARACTER*(JPDLEN) YBUFF
      CHARACTER*(JPDLEN) YTEMP
C
      CHARACTER*1  YSTATU
      CHARACTER*1  YACTCL
      CHARACTER*1  YACMOD
      CHARACTER*1  YDEBUG
      CHARACTER*1  YNOTRY
      CHARACTER*5  YREQNO
      CHARACTER*2  YHDRLN
      CHARACTER*2  YNOHDR
      CHARACTER*2  YNSHDR
      CHARACTER*2  YSUBER
      CHARACTER*2  YVERSN
      CHARACTER*4  YUSEID
      CHARACTER*4  YCOMID
      CHARACTER*4  YREASN
      CHARACTER*4  YRETCD
      CHARACTER*5  YMESLN
      CHARACTER*6  YSBTIM
      CHARACTER*6  YSBDAT
      CHARACTER*6  YRCTIM
      CHARACTER*6  YJOBSQ
      CHARACTER*7  YSENID
      CHARACTER*7  YRECID
      CHARACTER*7  YUSJOB
      CHARACTER*8  YUSACC
      CHARACTER*8  YACTON
      CHARACTER*10 YINT
      CHARACTER*10 YRESRV
      CHARACTER*96 YNAMWK
      CHARACTER*8  YFIDWK
      CHARACTER*96 YNMIBM
C
      DIMENSION YBLOCK(JPALEN)
C
      EQUIVALENCE (YBLOCK(1)( 1:2 ) , YHDRLN)
      EQUIVALENCE (YBLOCK(1)( 3:6 ) , YUSEID)
      EQUIVALENCE (YBLOCK(1)( 7:10) , YCOMID)
      EQUIVALENCE (YBLOCK(1)(11:17) , YSENID)
      EQUIVALENCE (YBLOCK(1)(18:24) , YRECID)
      EQUIVALENCE (YBLOCK(1)(25:30) , YSBDAT)
      EQUIVALENCE (YBLOCK(1)(31:36) , YSBTIM)
      EQUIVALENCE (YBLOCK(1)(37:42) , YJOBSQ)
      EQUIVALENCE (YBLOCK(1)(43:48) , YRCTIM)
      EQUIVALENCE (YBLOCK(1)(49:50) , YNOHDR)
      EQUIVALENCE (YBLOCK(1)(51:52) , YNSHDR)
      EQUIVALENCE (YBLOCK(1)(53:54) , YSUBER)
      EQUIVALENCE (YBLOCK(1)(55:55) , YSTATU)
      EQUIVALENCE (YBLOCK(1)(56:56) , YACTCL)
      EQUIVALENCE (YBLOCK(1)(57:61) , YMESLN)
      EQUIVALENCE (YBLOCK(1)(62:62) , YACMOD)
      EQUIVALENCE (YBLOCK(1)(63:70) , YUSACC)
      EQUIVALENCE (YBLOCK(1)(71:77) , YUSJOB)
      EQUIVALENCE (YBLOCK(1)(78:78) , YDEBUG)
      EQUIVALENCE (YBLOCK(1)(79:80) , YVERSN)
C
      EQUIVALENCE (YBLOCK(2)( 3:10) , YACTON)
      EQUIVALENCE (YBLOCK(2)(21:24) , YRETCD)
      EQUIVALENCE (YBLOCK(2)(25:28) , YREASN)
C