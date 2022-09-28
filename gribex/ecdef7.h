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
C                   ECMWF local GRIB use definition 7.
C
C                   Sensitivity gradient/Trajectory
C                   forecast and Sensitivity forecast data.
C                   ---------------------------------------
C
C
C                   Word
C                   ----
C                    38    as for definition 1.  
C
C                    39    Type  :
C                                 50 = Sensitivity gradient
C                                 51 = Trajectory forecast
C                                 52 = Sensitivity forecast
C
C                    40    Stream :
C                               1036 = Sensitivity forecasts
C
C                    41    as for definition 1.  
C
C                    42    Forecast number or diagnostic number:
C                          = 0 for trajectory forecast(type 51).
C                          = 0 for control sensitivity forecast(type 52).
C                          = 1 J1 diagnostic(type 50).
C                          = 2 J2 diagnostic(type 50).
C                          = 3 J3 diagnostic(type 50).
C                          = 4 J4 diagnostic(type 50).
C                          = 5 J5 diagnostic(type 50).
C
C                    43    Total number of diagnostics(type 50).
C                          0 for trajectory forecasts(type 51).
C                          Number of interations in diagnostic
C                          minimisation(type 52).
C
C                    44    Domain
C                          = 0 for Global
C                          = 1 for Europe
C                          = 2 for Northern Hemisphere
C                          = 3 for Southern Hemisphere
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
