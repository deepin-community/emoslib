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
      COMMON /BCMTAB/ NTABBTR(JTAB),NTABBS (JTAB),NTABBRV(JTAB),
     1                NTABBDW(JTAB),NTABDTR(JTAB),NTABDST(JTAB),
     2                NTABDL (JTAB),NTABDSQ(JTAB*20),NTABP(64,256)
C
C             NTABBTR    - TABLE B,  TABLE REFERENCE              ARRAY
C             NTABBS     - TABLE B,  SCALE                        ARRAY
C             NTABBRF    - TABLE B,  REFERENCE VALUE              ARRAY
C             NTABBDW    - TABLE B,  DATA WIDTH                   ARRAY
C             NTABDTR    - TABLE D,  TABLE REFERENCE              ARRAY
C             NTABDST    - TABLE D,  STARTING POINTERS            ARRAY
C             NTABDL     - TABLE D,  LENGTHS                      ARRAY
C             NTABDSQ    - TABLE D,  LIST OF SEQUENCE DESCRIPTORS ARRAY
C

