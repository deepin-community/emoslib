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
C     ECMWF local GRIB use definition 50.
C     Member State data.
C     ------------------
C
C     Octet     KSEC1(n)
C     -----     --------
C
C       5          2      Originating centre (WMO identifier)
C
C      26         22      Sub-centre identifier (= 98)
C
C      **         24      Flag field to indicate local use in Section 1 (= 1)
C
C      41         37      ECMWF local GRIB use definition identifier (= 50)
C
C      42         38      Class
C
C      43         39      Type
C
C     44-45       40      Stream
C
C     46-49       41      Version number/experiment identifier.
C                         (4 Ascii characters, right justified)
C
C      50         42      Number : Ensemble forecast number.
C                                  Control forecast is number 0,
C                                  perturbed forecasts 1-nn.
C                         Set to 0 if not ensemble forecast.
C
C      51         43      Total  : Total number of forecasts in ensemble.
C                                  This number includes the control forecast.
C                         Set to 0 if not ensemble forecast.
C
C      52         44      Model identifier (in range 1 - 255)
C
C     53-56       45      Latitude of North-west corner of area (degrees*10^6)
C
C     57-60       46      Longitude of North-west corner of area (degrees*10^6)
C
C     61-64       47      Latitude of South-east corner of area (degrees*10^6)
C
C     65-68       48      Longitude of South-east corner of area (degrees*10^6)
C
C     -----------------------------------------------------------------|
C
C     69-116    49-60     Reserved for ECMWF additions.
C
C      69         49      Original parameter number
C
C      70         50      Original parameter table number
C
C     71-116    51-60     Set to zero
C
C     -----------------------------------------------------------------|
C
C     117-300   61-106    Optional data.
C
C     -----------------------------------------------------------------|
C
C     ** Not stored in GRIB message.
C
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
