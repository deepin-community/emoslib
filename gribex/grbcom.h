C
C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C
C     Common blocks holding default or user supplied values.
C
C
C     REALs
C
#ifdef REAL_8
      REAL*8 FREF, FMAX
#else
      REAL*4 FREF, FMAX
#endif
      COMMON /GRBCOMR/
     X     FREF, FMAX
C
C     INTEGERs
C
      INTEGER NFREF, NFMAX, NRND, NDBG, NVCK, NONOFF, NOABORT
      INTEGER NUM2OK, NSUBCE, NEMOSLB, NEMOSET, N13FLAG,DUMPDATA
      COMMON /GRBCOMI/
     X        NFREF, NFMAX, NRND, NDBG, NVCK, NONOFF, NOABORT, NUM2OK
     X       ,NSUBCE, NEMOSLB, NEMOSET, N13FLAG,DUMPDATA
C
      INTEGER NEXT2O, NLOC2O
      COMMON /GRBCXT/ NEXT2O, NLOC2O
C
C     CHARACTERs
C
      CHARACTER*256 ELTPATH, ELBPATH,DUMPPATH
      COMMON /GRBELTP/ ELTPATH, ELBPATH,DUMPPATH
