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
C  ECMWF local GRIB use definition 19.
C  EFI - Extreme forecast index
C  ------------------------------------
C
C                 GRIB code
C     KSEC1(NN)   section 1
C                 octet(s)
C     ---------   ---------
C
C       37         41       ECMWF local GRIB use definition identifier
C                            = 19 = EFI, extreme forecast index
C
C       38         42       Class: 1 = Operations
C
C       39         43       Type : 27 = extreme forecast index
C                                  28 = extreme forecast index control
C
C       40        44-45     Stream: 1035 = Ensemble Forecast
C
C       41        46-49     Expver: Version number/experiment identifier.
C                                  (4 Ascii characters, right justified)
C
C       42         50       Zero, for compatibility with MARS labelling.
C
C       43         51       Ensemble size (0 - 255)
C
C       44         52       Power of 10 used to scale climate weight = F, say.
C
C       45        53-56     Weight * 10**F applied to climate month 1,
C                           stored as an integer
C
C       46        57-59     First month used to build climate month 1, YYYYMM
C
C       47        60-62     Last month used to build climate month 1, YYYYMM
C
C       48        63-65     First month used to build climate month 2, YYYYMM
C
C       49        66-68     Last month used to build climate month 2, YYYYMM
C
C       -         69-80     Set to zero.
C
C
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
