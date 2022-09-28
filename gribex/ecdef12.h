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
C     ECMWF local GRIB use definition 12.
C
C     Mean, average, etc.
C     -------------------
C
C     Octet     KSEC1(n)
C     -----     --------
C
C     1-3                 Length of section 1 in octets (=70)
C
C     4         1         Version number of code table 2
C
C     5         2         Identification of originating centre
C                         (98 = ECMWF)
C
C     6         3         Generating process identification number
C
C     7         4         Grid definition
C
C     8         5         Flag indicating whether sections 2/3 are present
C
C     9         6         Parameter indicator
C
C     10        7         Level indicator
C
C     11        8         Height/pressure level
C
C     12        9         Height/pressure level
C
C     13        10        Start date - Year (YY)
C
C     14        11        Start date - Month (MM)
C
C     15        12        Start date - Day (DD)
C
C     16        13        Start date - Hour (HH)
C
C     17        14        Start date - Minute (MM)
C
C     18        15        Time unit indicator (see WMO code table 4)
C
C     19        16        Time period P1
C
C     20        17        Time period P2
C
C     21        18        Time range indicator (see WMO code table 5)
C
C     22-23     19        Number included in the mean, average, etc.
C
C     24        20        Number missing from mean, average, etc.
C
C     25        21        Century of start date
C
C     26        22        Sub-centre identifier
C
C     27-28     23        Decimal scale factor
C
C               24        Flag to indicate ECMWF local usage follows
C                         in section 1
C
C     29-40     25-36     Zero
C
C     41        37        ECMWF local GRIB use definition identifier:
C                         12 = Mean, average, etc.
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
C     Description of the time period over which the mean/average/etc was
C     constructed:
C
C     50-53     42        Start date of the period (YYYYMMDD)
C
C     54-55     43        Start time of the period (HHMM)
C
C     56-59     44        Finish date of the period (YYYYMMDD)
C
C     60-61     45        Finish time of the period (HHMM)
C
C     62-65     46        Verifying date of the period (YYYYMMDD)
C
C     66-67     47        Verifying time of the period (HHMM)
C
C     68        48        Code showing method of meaning, averaging, etc:
C                          1  = by step of integration
C                          2  = by postprocessing step
C                          3  = monthly means of instantaneous values
C                          4  = monthly means of daily means
C                          5  = monthly means of forecast accumulations
C                          :
C                         255 = not used
C
C     69-70     49        Number (N) of different time intervals used to
C                         build data.
C                         N = 1 if a constant interval has been used.
C                         Otherwise all intervals must be given in the
C                         following list.
C
C     71-       50-       List of time intervals used (in order) in time
C     70+(N*4)  49+N      units defined in octet 18
C
C
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
