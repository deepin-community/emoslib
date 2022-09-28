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
C                   ECMWF local GRIB use definition 15.
C
C                   Seasonal forecast atmosphere data.
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
C                    40  Stream : 1090 = ECMWF seasonal forecasts
C
C                    41  Expver : Version number/experiment identifier.
C                                 ( 4 Ascii characters, right justified)
C
C                    42  Number : Ensemble member number.
C                                 Control forecast is number 0,
C                                 perturbed forecasts 1-nn.
C
C                    43  Zero
C
C                    44  System number: the "scientific version" number.
C
C                        0 = RD experiment
C                        1 - 65534 = operational version number
C                        65535 = missing
C
C                    45  Method number: distinguishes scientifically different
C                                       forecast ensembles (eg different
C                                       calibration/bias correction)
C
C                        0 = control integration (ie without data assimilation)
C                        1 - 65534 = operational version number
C                        65535 = missing
C
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
