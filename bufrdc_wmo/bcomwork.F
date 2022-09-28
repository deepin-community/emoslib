C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      COMMON /BCOMWORK/ RVIND,EPS,NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP
     2,                 NZZP,NSSP
C
C             NBPW          -  NUMBER OF BITS PER COMPUTER WORD
C             NWPT          -  WORD POINTER
C             NBPT          -  BIT POINTER
C             NWPTB         -  POINTER TO WORD AT THE BEGINING OF NEXT SECTION
C             NBPTB         -  POINTER TO BIT AT THE BEGINING OF NEXT SECTION
C             NMASK         -  BIT MASK ARRAY
C             NVIND         -  MISSING VALUE INDICATOR (INTEGER)
C             RVIND         -  MISSING VALUE INDICATOR (REAL)
C             NBENP         -  PREVIOUS BUFR EDITION NUMBER
C             NLTVNP        -  PREVIOUS LOCAL TABLE VERSION NUMBER
C             NXXP          -  BUFR MASTER TABLE USED
C             NYYP          -  VERSION NUMBER OF MASTER TABLE USED
C             NZZP          -  VERSION NUMBER OF LOCAL TABLE USED
C
C
