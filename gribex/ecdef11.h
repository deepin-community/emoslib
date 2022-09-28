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
C     ECMWF local GRIB use definition 11.
C     Supplementary data used by the analysis
C     ---------------------------------------
C
C     Octet     KSEC1(n)
C     -----     --------
C
C     41        37        ECMWF local GRIB use definition identifier:
C                         11 = Supplementary data used by the analysis
C
C     42        38        Class
C
C     43        39        Type
C
C     44-45     40        Stream
C
C     46-49     41        Version number/experiment identifier.
C                         (four ASCII characters, right justified)
C
C     Followed by details of the analysis which used the supplementary data:
C
C     50        42        Class of analysis
C
C     51        43        Type of analysis
C
C     52-53     44        Stream of analysis
C
C     54-57     45        Version number/experiment identifier of analysis
C                         (four ASCII characters, right justified)
C
C     58        46        Year of analysis (YY)
C
C     59        47        Month of analysis (MM)
C
C     60        48        Day of analysis (DD)
C
C     61        49        Hour of analysis (HH)
C
C     62        50        Minute of analysis (MM)
C
C     63        51        Century of analysis
C
C     64        52        Originating centre of analysis
C
C     65        53        Sub-centre of analysis
C
C     66-72     -         Spare (set to zero)
C
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
