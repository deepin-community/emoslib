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
C     Parameters
C
      INTEGER JP32, JPLONO, JPFFT, JPLOOK, JPMAX, JPMAXITER
      INTEGER JPMXTRY, JPTRNC, JPK, JPTRP1
      PARAMETER ( JP32 = 32 )
C
C     PARAMETER ( JPLONO = 2560 , JPFFT = 12000)  ! handles N1-N720, spectral truncations T1-T639
C     PARAMETER ( JPLONO = 6000 , JPFFT = 12000)
      PARAMETER ( JPLONO = 8200 , JPFFT = 12000)  ! [O|N]1280 require [5136|5120]+2 reals
      PARAMETER ( JPLOOK = 50)
C     PARAMETER ( JPMAX = 2048 )
      PARAMETER ( JPMAX = 2560 )
      PARAMETER ( JPMAXITER = 10)
      PARAMETER ( JPMXTRY = 3 )
      PARAMETER ( JPTRNC = 2047, JPK = (JPTRNC + 1)*(JPTRNC + 4) )
      PARAMETER ( JPTRP1 = (JPTRNC + 1) )
C
      REAL PPEPSA, PPQUART, PPHALF, PPTWO, PP90
      PARAMETER ( PPEPSA = 1.0E-6)
      PARAMETER ( PPQUART = 0.25E0)
      PARAMETER ( PPHALF = 0.5E0)
      PARAMETER ( PPTWO = 2.0E0)
      PARAMETER ( PP90 = 90.0E0)
C
      REAL PPI
      PARAMETER ( PPI = 3.14159265358979 )
C
C     Debug parameters
C
      INTEGER NDBG, NDBGLP
      COMMON /JDCNDBG/ NDBG, NDBGLP
