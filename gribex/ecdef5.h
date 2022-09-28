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
C                   ECMWF local GRIB use definition 5.
C                         Forecast probability data.
C                   ----------------------------------
C
C
C                    Words 38-41 as for definition 1.  
C                    Word 39 = 16 to indicate forecast probabilities.
C
C                    42  Forecast probability number
C
C                    43  Total number of forecast probabilities
C
C                    44  Threshold units decimal scale factor:
C                                 +/- power of 10 (or zero)
C                                 top bit = 1 for negative values
C
C                    45  Threshold indicator:
C                                 1 = only lower threshold present
C                                 2 = only upper threshold present
C                                 3 = both upper and lower thresholds
C                                     present
C
C                    46  Lower threshold value
C
C                    47  Upper threshold value
C
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C       
