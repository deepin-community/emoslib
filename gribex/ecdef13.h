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
C     ECMWF local GRIB use definition 13.
C     Wave 2D spectra direction and frequency
C     ---------------------------------------
C
C     Octet        KSEC1(n)
C     -----        --------
C
C     41           37           ECMWF local GRIB use definition identifier:
C                               13 = Wave 2D spectra direction and frequency
C
C     42           38           Class
C                               1 = Operations
C                               2 = Research
C
C     43           39           Type
C
C     44->45       40           Stream
C                               1045 = Wave
C
C     46->49       41           Version number/experiment identifier.
C                               (four ASCII characters, right justified)
C
C     50           42           As for MARS labelling
C                               (eg set to zero, or
C                                ensemble forecast number if appropriate)
C
C     51           43           As for MARS labelling
C                               (eg set to zero, or
C                                total number in ensembles if appropriate)
C
C     52           44           Direction number
C
C     53           45           Frequency number
C
C     54           46           Total number of directions (Nd)
C
C     55           47           Total number of frequencies (Nf)
C
C     56->59       48           Integer scaling factor applied to directions
C                               in following list of direction definitions
C                               (4-byte integer)
C
C     60->63       49           Integer scaling factor applied to frequencies
C                               in following list of frequency definitions
C                               (4-byte integer)
C
C     64            -           Flag to show whether or not system and method
C                               number are present:
C                                 0 = not present (old style product)
C                                 1 = present
C
C     65->66      (50+Nd+Nf)    System number: the "scientific version" number.
C                                 0 = RD experiment
C                                 1 -> 65534 = operational version number
C                                 65535 = missing
C
C     67->68      (51+Nd+Nf)    Method number: distinguishes scientifically
C                               different forecast ensembles (eg different
C                               calibration/bias correction)
C                                 0 = control integration
C                                    (ie without data assimilation)
C                                 1 -> 65534 = operational version number
C                                 65535 = missing
C
C     69->100                   Spare (set to zero)
C
C    101->         50->         List of Nd scaled directions
C   (100+Nd*4)    (49+Nd)       (4-byte integers)
C
C   (101+Nd*4)->  (50+Nd)->     List of Nf scaled frequencies
C (100+Nd*4+Nf*4) (49+Nd+Nf)    (4-byte integers)
C
C
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
