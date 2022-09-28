C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C
C
C     ECMWF ocean data GRIB section 2
C     -------------------------------
C
C     Octet     KSEC2(n)
C     -----     --------
C
C     1-3       -         Length of section
C
C     4         -         Not used: set to zero
C
C     5         -         255 (all bits set to 1)
C
C     6         1         Data representation type = 192 = ECMWF ocean grid
C
C     7-8       2         Ni - number of points along the first axis
C                         (32767 = not used)
C
C     9-10      3         Nj - number of points along the second axis
C                         (32767 = not used)
C
C     11-27     -         Not used: set to zero
C
C     -         4-10      Reserved. Set to 0.
C
C     28        11        Scanning mode flags (see WMO code table 8)
C
C     29-32     -         Not used: set to zero
C
C     -         12-22     Reserved. Set to 0.
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
