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
C     ECMWF local GRIB use definition 190.
C     Multiple ECMWF local definitions
C     --------------------------------
C
C     Octet     KSEC1(n)
C     -----     --------
C
C      41         37      ECMWF local GRIB use definition identifier (= 190)
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
C      52         44      Number of ECMWF local definitions (N, say)
C
C      53         45      First ECMWF local definition number
C
C     54-55       46      Number of bytes in first ECMWF local definition
C
C      56         47      Second ECMWF local definition number
C
C     57-58       48      Number of bytes in second ECMWF local definition
C
C      ...        ...
C
C    50+N*3      43+N*2   Nth ECMWF local definition number
C
C  51+N*3-52+N*3 44+N*2   Number of bytes in Nth ECMWF local definition
C
C
C   N ECMWF local definitions follow:
C
C   53+N*3...    45+N*2   Stream of ECMWF local definition bytes previously
C                         encoded/decoded via function calls.
C
C   The ECMWF local definitions are the locally defined extensions from
C   byte 41 onwards.
C
C   If definition 190 appears in the list of ECMWF local, the stream of
C   ECMWF local definition bytes will include its own stream of ECMWF
C   local definition bytes.
C
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
