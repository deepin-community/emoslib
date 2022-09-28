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
C     ECMWF local GRIB use definition 10.
C     EPS tubes
C     ---------
C
C     Octet     KSEC1(n)
C     -----     --------
C
C     41        37        ECMWF local GRIB use definition identifier:
C                         10 = EPS tubes
C
C     42        38        Class
C                         1 = Operations
C                         2 = Research
C
C     43        39        Type
C                         24 = Tubes
C
C     44-45     40        Stream
C                         1035 = Ensemble forecasts
C
C     46-49     41        Version number/experiment identifier.
C                         (four ASCII characters, right justified)
C
C     50        42        Tube number (0=central cluster)
C
C     51        43        Total number of tubes (excluding central cluster)
C
C     52        44        Central cluster definition
C                         1: radius = % of total variance
C                         2: radius = predefined value
C
C     53        45        Indicator of parameter considered
C                         (see code table 2 in section 1)
C
C     54        46        Indicator of type of level considered
C                         (see WMO code table 3)
C
C     55-57     47        Northern latitude of the domain of tubing
C
C     58-60     48        Western longitude of the domain of tubing
C
C     61-63     49        Southern latitude of the domain of tubing
C
C     64-66     50        Eastern longitude of the domain of tubing
C
C     67        51        Number of tube to which the operational
C                         forecast belongs(*)
C                           0 = central cluster,
C                         254 = does not belong to any tube
C
C     68        52        Number of tube to which the control
C                         forecast belongs(*)
C                           0 = central cluster,
C                         254 = does not belong to any tube
C
C     69-70     53        Height/pressure of level considered
C                         (see WMO code table 3)
C
C     71-72     54        Reference step considered
C                         (same units of time as forecast timesteps)
C
C     73-74     55        Radius of central cluster
C                         (in units of parameter defined in element 45)
C
C     75-76     56        Ensemble standard deviation
C                         (in units of parameter defined in element 45)
C
C     77-78     57        Distance of the tube extreme to the ensemble mean
C                         (in units of parameter defined in element 45). Not
C                         applicable if this is the central cluster, in which
C                         case the value is set = 65535, ie a missing value.
C
C     79        58        Number of forecasts belonging to the tube
C                         or central cluster, including the control
C                         forecast (N)
C
C     80-(79+N) 59-(58+N) List of N ensemble forecast numbers (**)
C                         Order is important, first on the list is
C                         the tube extreme
C
C     Notes:
C     *   a forecast may belong to several tubes. In this case, the
C         forecast is associated with the tube whose extreme is closest.
C
C     **  by order of decreasing distance to the ensemble mean.
C
C
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
