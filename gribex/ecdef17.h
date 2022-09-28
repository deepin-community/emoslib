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
C                   ECMWF local GRIB use definition 17.
C                   Surface temperature or sea-ice data.
C                   ------------------------------------
C
C                 GRIB code
C     KSEC1(NN)   section 1
C                 octet(s)
C     ---------   ---------
C
C       37         41        ECMWF local GRIB use definition identifier
C                            = 17
C                            = surface temperature fields made from SST and
C                            SSMI data with first-guess values inserted on
C                            land points.
C
C       38         42        Class : 1 = Operations
C
C       39         43        Type  : 2 = Analysis
C
C       40         44-45     Stream : 1025 = Daily archive
C
C       41         46-49     Expver : Version number/experiment identifier.
C                                   (4 Ascii characters, right justified)
C
C       42         50        Zero, for compatibility with MARS labelling.
C
C       43         51        Zero, for compatibility with MARS labelling.
C
C       44         52-54     Date of SST field used; YYYYMMDD.
C                            (Stored in 3 bytes as YYYYMMDD-19000000.
C                             Zero = date not given.)
C
C       45         55        Type of SST field used:
C                            0 = climatology
C                            1 = 1/1 degree sst data
C                            2 = 2/2 degree sst data
C
C       46         56        Count of ICE fields used (n, say).
C                            (NB. n maybe 0).
C
C       47         57-59     Date of first ICE field used; YYYYMMDD.
C                            (Stored in 3 bytes as YYYYMMDD-19000000.
C                             Zero = date not given.)
C
C       48         60        First day satellite number (ICE data)
C
C       49         61-63     Date of second ICE field used ; YYYYMMDD.
C                            (Stored in 3 bytes as YYYYMMDD-19000000.
C                             Zero = date not given.)
C
C       50         64        Second day satellite number (ICE data)
C
C       51         65-67     Date of third ICE field used ; YYYYMMDD.
C                            (Stored in 3 bytes as YYYYMMDD-19000000.
C                             Zero = date not given.)
C
C       52         68        Third day satellite number (ICE data)
C
C
C       ...
C
C       45+(n*2)   53+(n*4)  Date of nth field used; YYYYMMDD.
C                ->55+(n*4)  (Stored in 3 bytes as YYYYMMDD-19000000.
C                             Zero = date not given.)
C
C       46+(n*2)   56+(n*4)  Nth  day satellite number (ICE data)
C
C       Slots for dates are allocated 10 at a time, so some slots may be
C       empty (ie zero). Empty slots are not includedd in the count N.
C
C
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
