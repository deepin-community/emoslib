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
C     ECMWF local GRIB use definition 14.
C     Brightness temperature.
C     -----------------------
C
C     Octet        KSEC1(n)
C     -----        --------
C
C     41           37           ECMWF local GRIB use definition identifier:
C                               14 = Brightness temperature.
C
C     42           38           Class
C                               1 = Operations
C                               2 = Research
C
C     43           39           Type
C                               12 = estimate of forecast accuracy (ef)
C                               13 = estimate of analysis accuracy (ea)
C
C     44->45       40           Stream
C                               1025 = daily archive
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
C     52           44           Channel number
C
C     53->56       45           Integer scaling factor applied to frequencies
C                               in following list of frequency definitions
C                               (4-byte integer)
C
C     57           46           Total number of frequencies (Nf)
C
C     58-60                     Spare (set to zero)
C
C     61->         47->         List of Nf scaled frequencies
C     60+Nf*4      46+Nf        (4-byte integers)
C
C     61+Nf*4->                 Spare (set to zero)
C     1080
C
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
