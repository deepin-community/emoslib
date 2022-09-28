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
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C     ECMWF local GRIB use definition 9.
C     Singular vectors and ensemble perturbations.
C     ---------------------------------------------
C
C     Octet     KSEC1(n)
C     -----     --------
C
C     42        38        Class : 1 = Operations
C                                 2 = Research
C
C     43        39        Type  : 60 = Perturbed analysis
C                                 62 = Singular vector
C                                 63 = Adjoint singular vector
C
C     44-45     40        Stream : 1035 = Ensemble forecasts
C
C     46-49     41        Expver : Version number/experiment identifier.
C                                  ( 4 Ascii characters, right justified)
C
C     50-51     42        If type 60, perturbed ensemble forecast number.
C                         If type 62 or 63, singular vector number.
C
C
C     If type = 60:
C
C     52-92     43-55     Set to zero.
C
C
C     If type = 62 or 63:
C
C     52-53     43        Number of iterations.
C
C     54-55     44        Number of singular vectors computed.
C
C     56        45        Norm used at initial time.
C
C     57        46        Norm used at final time.
C
C     58-61     47        Multiplication factor to convert latitude/longitude
C                         and accuracy from real to integer.
C
C     62-65     48        Latitude in degrees of north-west corner of LPO area
C                         multiplied by KSEC1(47).
C
C     66-69     49        Longitude in degrees of north-west corner of LPO area
C                         multiplied by KSEC1(47).
C
C     70-73     50        Latitude in degrees of south-east corner of LPO area
C                         multiplied by KSEC1(47).
C
C     74-77     51        Longitude in degrees of south-east corner of LPO area
C                         multiplied by KSEC1(47).
C
C     78-81     52        Accuracy multiplied by KSEC1(47).
C
C     82-83     53        Number of singular vectors evolved.
C
C     84-91     54-55     Ritz numbers.
C                         Given ritz, then:
C                           KSEC1(54) = NINT( LOG10(RITZ)-5 )
C                           KSEC1(55) = NINT( RITZ/ EXP(LOG(10.0)*KSEC1(54) ) )
C                         Thus:
C                           RITZ = KSEC1(55)*EXP( LOG(10.0)*KSEC1(54) )
C
C     92                  Reserved. Set to zero.
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
