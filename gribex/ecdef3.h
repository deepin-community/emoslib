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
C                   ECMWF local GRIB use definition 3.
C                         Satellite image data.
C                   ---------------------------------
C
C         Words 38-41 as for definition 1.
C
C                    42  Band  : 0 = first infrared band
C                                1 = second infrared band
C                               10 = first visible band
C                               20 = water vapour
C                            100+i = ith spectral band
C
C                    43 Function code : Interpretation of pixel value.
C                                0 = value is pixel value
C                                1 = value is temperatures in degrees K,
C                                    and is 145 + pixel value.
C                               255 = translation table follows
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
