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
      COMMON /BCOMREQ/ RQVAL(JELEM),NREQ(2),NRQ(JELEM),NRQL
C
C             *NREQ*    -  FLAG
C                          BIT NUMBER     MEANING
C
C                              1        - 0 NO BIT MAP DELIVERED TO USER
C                                         1    BIT MAP DELIVERED TO USER
C                              2        - 0 NO PARTIAL EXPANSION
C                                         1    PARTIAL EXPANSION
C                              3        - 0 NO Q/C REQUIRED
C                                       - 1    Q/C REQUIRED
C                              4        - 0 NO STATISTICS REQUIRED
C                                       - 1    STATISTICS
C                              5        - 0 NO DIFFRENCE STATISTICS
C                                       - 1    DIFFERENCE STATISTICS
C                              6        - 0 NO SUBSTITUTED VALUES
C                                       - 1    SUBSTITUTED VALUES
C             *NRQL*    -  NUMBER OF REQUESTED ELEMENTS
C             *NRQ*     -  LIST OF REQUESTED TABLE B REFERENCE
C             *RQVAL*   -  LIST OF VALUES SIGNIFYING REQUESTED ELEMENT
C                          (SAY PRESSURE  AT 50000 PA)
C

