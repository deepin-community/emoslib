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
C     ECMWF local GRIB use definition 16.
C
C     Seasonal forecast monthly mean data.
C     ------------------------------------
C
C     Octet     KSEC1(n)
C     -----     --------
C
C     41           37    16 = ECMWF seasonal forecast monthly mean data
C
C     42           38    Class : 1 = Operations
C                                2 = Research
C
C     43           39    Type  : 80 = Forecast seasonal mean
C                                81 = Forecast seasonal maximum
C                                82 = Forecast seasonal minimum
C                                83 = Forecast seasonal standard
C                                     deviation
C
C     44-45        40    Stream : 1091 = ECMWF seasonal forecast
C                                        monthly means
C
C     46-49        41    Expver : Version number/experiment identifier
C                                 (4 Ascii characters, right justified)
C
C     50-51        42    Number : Ensemble member number.
C                                 Control forecast is number 0,
C                                 perturbed forecasts 1-nn.
C
C                  43    Zero (for compatibity with ensemble numering)
C
C     52-53        44    System number: the "scientific version" number.
C                          0 = RD experiment
C                          1 - 65534 = operational version number
C                          65535 = missing
C
C     54-55        45    Method number:
C                          distinguishes scientifically different
C                          forecast ensembles
C                          (eg different calibration/bias correction)
C                          0 = control integration (without data
C                              assimilation)
C                          1 - 65534 = operational version number
C                          65535 = missing
C
C     56-59        46    Verifying month (in format YYYYMM)
C
C     60           47    Averaging period (eg 6-hour, 24-hour)
C
C     61-80              Spare (set to zero)
C
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
