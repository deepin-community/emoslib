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
C     ECMWF local GRIB use definition 191.
C     Free format data descriptor.
C     ----------------------------
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
C      41         37      ECMWF local GRIB use definition identifier (= 191)
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
C     50-51      42-43    Set to zero (For compatibility with other
C                                      ECMWF local definitions)
C
C      52         44      Format version major number
C
C      53         45      Format version minor number
C
C      54         46      Original sub-centre identifier
C
C     55-58     (47-50)   Set to zero
C
C     59-60       51      Number of bytes of free format data descriptor
C                         (N, say)
C
C   61-(60+N) 52-(51+N/J) Data descriptor bytes packed in integer array
C                         elements
C                         (J = the number of bytes in an INTEGER = 4 or 8)
C
C
C     The section will be padded with zeroes to make the overall length
C     of the section = 60 + 80*M for some M > 0.
C
C     -----------------------------------------------------------------|
C
C     ** Not stored in GRIB message.
C
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
