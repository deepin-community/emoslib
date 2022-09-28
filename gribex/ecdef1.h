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
C                   ECMWF local GRIB use definition 1.
C                         Ensemble forecast data.
C                   ----------------------------------
C
C
C                    38    Class : 1 = Operations
C                                  2 = Research
C
C                    39    Type  : 1 = First Guess
C                                  2 = Analysis
C                                  3 = Initialised analysis
C                                  4 = OI analysis
C                                  5 = 3 D variational analysis
C                                  6 = 4 D variational analysis
C                                  7 = 3 D variational gradients
C                                  8 = 4 D variational gradients
C                                  9 = Forecast
C                                 10 = Control forecast
C                                 11 = Perturbed forecast
C                                 12 = Errors in first guess
C                                 13 = Errors in analysis
C                                 14 = Cluster means
C                                 15 = Cluster standard deviations.
C                                 20 = Climatology
C                                 30 = Observations
C                                 31 = Quality control
C                                 32 = Difference statistics
C                                 40 = Image data
C
C                    40  Stream : 1-1022 = Satellite data stream is the
C                                          satellite number as per BUFR
C                                          code table 0 01 007.
C                                              50 = Meteosat 3
C                                              51 = Meteosat 4
C                                              52 = Meteosat 5
C                                              53 = Meteosat 6
C                                             201 = NOAA 9
C                                             250 = GOES 6
C                                             251 = GOES 7
C                               1025 = Daily archive
C                               1035 = Ensemble forecasts
C                               1036 = Sensitivities
C                               1041 = TOGA
C                               1042 = Chernobyl
C                               1043 = Monthly means of daily archive
C                               1044 = Supplementary data
C                               1045 = Wave
C                               1046 = Ocean
C                               1047 = FGGE
C                               1050 = Bracknell (daily)
C                               1051 = Washington (daily)
C                               1052 = Offenbach (daily)
C                               1053 = Paris (daily)
C                               1054 = Tokyo (daily)
C                               1055 = Montreal (daily)
C                               1056 = Melbourne (daily)
C                               1060 = Test
C                               1070 = Monthly standard deviation and covariance
C                               1071 = Monthly means of daily means
C                               1080 = Wave monthly means of daily archive
C                               1081 = Wave EPS
C                               1090 = ECMWF ensemble seasonal forecasts
C                               2231 = Meteo France climate centre
C                               2232 = MPI climate centre
C                               2233 = UKMO climate centre
C                               2240 = ECMWF seasonal forecast
C                               2241 = Meteo France seasonal forecast
C                               2242 = EDF seasonal forecast
C                               2243 = UKMO seasonal forecast
C
C                    41  Expver : Version number/experiment identifier.
C                                 ( 4 Ascii characters, right justified)
C
C                    42  Number : Ensemble forecast number.
C                                 Control forecast is number 0,
C                                 perturbed forecasts 1-nn.
C
C                                 Set to 0 if not ensemble forecast.
C
C                    43  Total  : Total number of forecasts in ensemble.
C                                 This number includes the control
C                                 forecast.
C
C                                 Set to 0 if not ensemble forecast.
C                                 Set to 0 if seasonal (ocean) data
C                                 Set to 33 if ensemble mean/standard
C                                 deviation.
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
