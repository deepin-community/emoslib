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
      COMMON /BCMTABLOAD/ MTABBTR(JTAB,JTMAX),MTABBS (JTAB,JTMAX),
     1                MTABBRV(JTAB,JTMAX),
     1                MTABBDW(JTAB,JTMAX),MTABDTR(JTAB,JTMAX),
     2                MTABDST(JTAB,JTMAX),
     2                MTABDL (JTAB,JTMAX),MTABDSQ(JTAB*20,JTMAX),
     2                MTABP(JTCLAS,JTEL,JTMAX), NTC,NTT
C     
C             MTABBTR    - TABLE B,  TABLE REFERENCE              ARRAY
C             MTABBS     - TABLE B,  SCALE                        ARRAY
C             MTABBRF    - TABLE B,  REFERENCE VALUE              ARRAY
C             MTABBDW    - TABLE B,  DATA WIDTH                   ARRAY
C             MTABDTR    - TABLE D,  TABLE REFERENCE              ARRAY
C             MTABDST    - TABLE D,  STARTING POINTERS            ARRAY
C             MTABDL     - TABLE D,  LENGTHS                      ARRAY
C             MTABDSQ    - TABLE D,  LIST OF SEQUENCE DESCRIPTORS ARRAY
C             NTT        - NUMBER OF TABLES
C             NTC        - CURRENT TABLE
C
C

