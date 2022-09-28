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
C     ECMWF local GRIB use definition 21.
C     Sensitive area predictions
C     -----------------------------------
C
C     Octet     KSEC1(n)
C     -----     --------
C
C     42        38        Class
C
C     43        39        Type
C
C     44-45     40        Stream
C
C     46-49     41        Expver
C
C     50-51     42        Forecast number or singular vector number. Zero for
C                         analysis.
C
C
C     If type = 60 (perturbed analysis):
C
C     52-93     43-57     Set to zero.
C
C
C     For other types, the area definition (octets 58 to 77) should always be
C     coded. Other octets from 53 to 93 may be coded as zeroes.
C
C     Allowed ranges are [-90,90] for latitudes and [0,360] for longitudes.
C
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
C     62-65     48        Latitude in degrees of north-west corner of forecast
C                         verification area multiplied by KSEC1(47).
C
C     66-69     49        Longitude in degrees of north-west corner of forecast
C                         verification area multiplied by KSEC1(47).
C
C     70-73     50        Latitude in degrees of south-east corner of forecast
C                         verification area multiplied by KSEC1(47).
C
C     74-77     51        Longitude in degrees of south-east corner of forecast
C                         verification area multiplied by KSEC1(47).
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
C     92        56        For all types, this is the period in hours between
C                         the time when the targeted observations are taken
C                         (the reference time in KSEC1(10:14)) and the forecast
C                         verification time.
C                         For singular vectors, this is the optimisation time.
C
C     93        57        Forecast lead time (hours)
C                         For all types, this is the lead time of forecast(s)
C                         on which sensitive area prediction is based. The lead
C                         time is the time from the initialization of the
C                         forecast to the reference time in KSEC1(10:14).
C
C     94        58        WMO identifier of the centre from which the data
C                         originated
C
C     95-96     59        Method number
C
C     97-98     60        Total number of forecasts in ensemble (or zero if
C                         not appropriate).
C
C     99        61        Shape of verification region:
C                         0: lat-lon box as given by KSEC1(48-51)
C                         1: circular region with centre at
C                         latitude  = 0.5*(KSEC1(48)+KSEC1(50))/KSEC1(47) (deg)
C                         longitude = 0.5*(KSEC1(49)+KSEC1(51))/KSEC1(47) (deg)
C                         and with
C                         radius = 0.5*(KSEC1(48)-KSEC1(50))/(KSEC1(47)*111.199)
C                                = great-circle distance in km
C
C                         This is the largest circle fitting in a lat-lon box
C                         defined by KSEC1(48-51).

C
C     100                 Reserved. Set to zero.
C
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
